package app.playback

import app.domain.{CompiledTrack, TimingContext, Track}
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.effect.{FiberIO, IO, Ref, Resource}
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.{Files, Path, StandardWatchEventKinds, WatchEvent, WatchService}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Watches a directory containing Scala track definitions. A new file, a deletion, or a modification triggers a full
  * scan and a rebuild of the active playback.
  */
trait TrackDirectoryMonitor {
  def scanOnce: IO[List[Track]]
  def start: IO[Unit]
  def stop: IO[Unit]
}

object TrackDirectoryMonitor {

  type TrackParser = Path => ValidatedNec[String, Track]

  private[playback] def requiresScan(events: Iterable[WatchEvent[_]]): Boolean =
    events.exists { event =>
      event.kind == StandardWatchEventKinds.OVERFLOW ||
      (event.context match {
        case path: Path => path.getFileName.toString.toLowerCase.endsWith(".scala")
        case _          => false
      })
    }

  def live(
    directory: Path,
    parser: TrackParser,
    compiler: Track => CompiledTrack,
    playback: PlaybackController,
    timing: TimingContext,
    policy: RepeatPolicy = RepeatPolicy.none,
    logger: Logger[IO] = Slf4jLogger.getLogger[IO],
    pollInterval: FiniteDuration = 500.millis
  ): TrackDirectoryMonitor =
    new FileSystemTrackDirectoryMonitor(directory, parser, compiler, playback, timing, policy, logger, pollInterval)

  private final class FileSystemTrackDirectoryMonitor(
    directory: Path,
    parser: TrackParser,
    compiler: Track => CompiledTrack,
    playback: PlaybackController,
    timing: TimingContext,
    policy: RepeatPolicy,
    logger: Logger[IO],
    pollInterval: FiniteDuration
  ) extends TrackDirectoryMonitor {

    private val watcherRef: Ref[IO, Option[FiberIO[Unit]]] = Ref.unsafe(None)

    override def scanOnce: IO[List[Track]] =
      trackFiles.flatMap { files =>
        if (files.isEmpty) logger.info(s"No track files found in $directory").as(List.empty[Track])
        else files.traverse(loadTrack).flatMap { results =>
          val errors = results.collect { case Left(error) => error }
          val tracks = results.collect { case Right(track) => track }

          errors.traverse_(error => logger.error(error)) *>
            (if (tracks.nonEmpty)
               logger.info(s"Loaded ${tracks.size} track(s) from $directory") *>
                 playback.replace(tracks, timing, policy)
             else IO.unit) *>
            IO.pure(tracks)
        }
      }

    override def start: IO[Unit] =
      watcherRef.get.flatMap {
        case Some(_) => IO.unit
        case None =>
          logger.info(s"Starting track monitor for $directory (poll interval: $pollInterval)") *>
            watchLoop.start.flatMap(fiber => watcherRef.set(Some(fiber)))
      }

    override def stop: IO[Unit] =
      watcherRef.getAndSet(None).flatMap(_.fold(IO.unit)(_.cancel)) *>
        logger.info(s"Track monitor stopped for $directory")

    private def trackFiles: IO[List[Path]] =
      IO.blocking(Files.exists(directory)).flatMap {
        case false => logger.warn(s"Track directory does not exist: $directory").as(List.empty)
        case true =>
          Resource
            .fromAutoCloseable(IO.blocking(Files.list(directory)))
            .use(stream => IO.blocking(stream.iterator().asScala.filter(Files.isRegularFile(_)).filter(isScalaFile).toList))
      }

    private def loadTrack(path: Path): IO[Either[String, Track]] =
      IO.blocking {
        parser(path) match {
          case Valid(track) if compiler(track).events.forall(_.isRight) => Right(track)
          case Valid(_) => Left(s"Track compilation failed for $path")
          case Invalid(errors) => Left(s"Track parse failed for $path: ${errors.toList.mkString(", ")}")
        }
      }

    private def watchLoop: IO[Unit] =
      watchServiceResource.use { watchService =>
        IO.blocking(
          directory.register(
            watchService,
            StandardWatchEventKinds.ENTRY_CREATE,
            StandardWatchEventKinds.ENTRY_MODIFY,
            StandardWatchEventKinds.ENTRY_DELETE
          )
        ) *>
          logger.info(s"Watch service registered for $directory") *>
          waitForChanges(watchService).foreverM
      }

    private def waitForChanges(watchService: WatchService): IO[Unit] =
      IO.interruptibleMany(watchService.take()).flatMap { key =>
        val shouldScan = requiresScan(key.pollEvents().asScala)
        IO.blocking(key.reset()).void *>
          (if (shouldScan) IO.sleep(pollInterval) *> discardPendingEvents(watchService) *> scanOnce.void else IO.unit)
      }

    private def discardPendingEvents(watchService: WatchService): IO[Unit] =
      IO.blocking {
        Iterator
          .continually(watchService.poll())
          .takeWhile(_ != null)
          .foreach { key =>
            key.pollEvents()
            key.reset()
          }
      }

    private def watchServiceResource: Resource[IO, WatchService] =
      Resource.fromAutoCloseable(IO.blocking(directory.getFileSystem.newWatchService()))

    private def isScalaFile(path: Path): Boolean =
      path.getFileName.toString.toLowerCase.endsWith(".scala")
  }
}
