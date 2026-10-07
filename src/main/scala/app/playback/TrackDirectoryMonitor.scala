package app.playback

import app.domain.DomainError
import app.domain.PlaybackPlan
import app.domain.TimingContext
import app.domain.Track
import app.domain.Tracks
import app.syntax.flatten
import cats.data.Ior
import cats.data.IorNec
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec
import cats.effect.FiberIO
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.nio.file.WatchEvent
import java.nio.file.WatchService
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** Watches a directory containing Scala track definitions. A new file, a deletion, or a modification triggers a full
  * scan and a rebuild of the active playback.
  */
trait TrackDirectoryMonitor {
  def scanOnce: IO[PlaybackPlan]
  def start: IO[Unit]
  def stop: IO[Unit]
}

object TrackDirectoryMonitor {

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
    playback: PlaybackController,
    timing: TimingContext,
    musicFile: Path,
    policy: RepeatPolicy = RepeatPolicy.none,
    logger: Logger[IO] = Slf4jLogger.getLogger[IO],
    pollInterval: FiniteDuration
  ): TrackDirectoryMonitor =
    new FileSystemTrackDirectoryMonitor(
      directory,
      playback,
      timing,
      musicFile,
      policy,
      logger,
      pollInterval
    )

  private final class FileSystemTrackDirectoryMonitor(
    directory: Path,
    playback: PlaybackController,
    timing: TimingContext,
    musicFile: Path,
    policy: RepeatPolicy,
    logger: Logger[IO],
    pollInterval: FiniteDuration
  ) extends TrackDirectoryMonitor {

    private val watcherRef: Ref[IO, Option[FiberIO[Unit]]] = Ref.unsafe(None)
    private val tracksRef: Ref[IO, PlaybackPlan]           = Ref.unsafe(PlaybackPlan.empty)

    def logErrors(errors: NonEmptyChain[DomainError]): IO[Unit] =
      errors.traverse_(error => logger.error(error.toString))

    private def replacePlanIfChanged(plan: PlaybackPlan): IO[PlaybackPlan] = {
      tracksRef.get.flatMap { current =>
        if current == plan then IO.pure(current)
        else playback.replace(plan, policy) *> tracksRef.set(plan) *> IO.pure(plan)
      }
    }

    private def loadPlanFromMusic(trackSourceFiles: Seq[Path]): IO[IorNec[DomainError, PlaybackPlan]] =
      IO.blocking(Files.exists(musicFile)).flatMap {
        case false =>
          IO.pure(Ior.left(NonEmptyChain.one(DomainError.TrackFileParseFailed(s"Music file not found: $musicFile"))))
        case true =>
          IO.blocking {
            TrackFileParser.compileAndEvaluateFile[ValidatedNec[String, Seq[Track]]](
              scalaFile = musicFile.toString,
              className = "Music",
              methodName = "playWrapper",
              sourceFiles = trackSourceFiles.map(_.toString)
            )
          }.map {
            case Validated.Valid(musicTracks) =>
              Tracks
                .from(musicTracks)
                .map(tracks => PlaybackPlan.fromTracks(tracks, timing))
                .flatten
            case Validated.Invalid(musicErrors) =>
              Ior.left(musicErrors)
          }
      }

    override def scanOnce: IO[PlaybackPlan] =
      trackFiles().flatMap { trackSources =>
        loadPlanFromMusic(trackSources)
          .flatMap {
            case Ior.Left(errors)       => logErrors(errors) *> tracksRef.get
            case Ior.Right(plan)        => replacePlanIfChanged(plan) *> IO.pure(plan)
            case Ior.Both(errors, plan) => logErrors(errors).as(plan)
          }
      }

    override def start: IO[Unit] =
      watcherRef.get.flatMap {
        case Some(_) => IO.unit
        case None =>
          logger.debug(s"Starting track monitor for $directory (poll interval: $pollInterval)") *>
            watchLoop.start.flatMap(fiber => watcherRef.set(Some(fiber)))
      }

    override def stop: IO[Unit] =
      watcherRef.getAndSet(None).flatMap(_.fold(IO.unit)(_.cancel)) *>
        logger.debug(s"Track monitor stopped for $directory")

    private def trackFiles(): IO[Seq[Path]] =
      IO.blocking(Files.exists(directory)).flatMap {
        case false => logger.error(s"Track directory does not exist: $directory").as(Seq.empty)
        case true =>
          Resource
            .fromAutoCloseable(IO.blocking(Files.list(directory)))
            .use(stream =>
              IO.blocking(
                stream
                  .iterator()
                  .asScala
                  .filter(Files.isRegularFile(_))
                  .filter(isScalaFile)
                  .filterNot(isConfiguredMusicFile)
                  .filterNot(isMusicDefinitionFile)
                  .toSeq
              )
            )
      }

    private def isConfiguredMusicFile(path: Path): Boolean =
      path.toAbsolutePath.normalize() == musicFile.toAbsolutePath.normalize()

    private def isMusicDefinitionFile(path: Path): Boolean =
      path.getFileName.toString.equalsIgnoreCase("Music.scala")

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
          waitForChanges(watchService).foreverM
      }

    private def waitForChanges(watchService: WatchService): IO[Unit] =
      IO.interruptibleMany(watchService.take()).flatMap { key =>
        val shouldScan = requiresScan(key.pollEvents().asScala)
        IO.blocking(key.reset()).void *>
          (if (shouldScan)
             IO.sleep(pollInterval) *>
               discardPendingEvents(watchService) *>
               logger.debug(s"Detected changes in $directory, rescanning...") *>
               scanOnce.void
           else IO.unit)
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
