package app.playback

import app.domain.DomainError
import app.domain.PlaybackPlan
import app.domain.TimingContext
import app.domain.Track
import app.domain.Tracks
import app.syntax.flatten
import app.syntax.sequenceIO
import cats.data.Ior
import cats.data.IorNec
import cats.data.NonEmptyChain
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
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
  def scanOnce(logErrorsOnly: Boolean = true): IO[PlaybackPlan]
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
    parser: Path => ValidatedNec[DomainError, Track],
    compiler: Tracks => IorNec[DomainError, PlaybackPlan],
    playback: PlaybackController,
    timing: TimingContext,
    musicFile: Option[Path] = None,
    policy: RepeatPolicy = RepeatPolicy.none,
    logger: Logger[IO] = Slf4jLogger.getLogger[IO],
    pollInterval: FiniteDuration
  ): TrackDirectoryMonitor =
    new FileSystemTrackDirectoryMonitor(
      directory,
      parser,
      compiler,
      playback,
      timing,
      musicFile,
      policy,
      logger,
      pollInterval
    )

  private final class FileSystemTrackDirectoryMonitor(
    directory: Path,
    parser: Path => ValidatedNec[DomainError, Track],
    compiler: Tracks => IorNec[DomainError, PlaybackPlan],
    playback: PlaybackController,
    timing: TimingContext,
    musicFile: Option[Path],
    policy: RepeatPolicy,
    logger: Logger[IO],
    pollInterval: FiniteDuration
  ) extends TrackDirectoryMonitor {

    private val watcherRef: Ref[IO, Option[FiberIO[Unit]]] = Ref.unsafe(None)
    private val tracksRef: Ref[IO, PlaybackPlan]           = Ref.unsafe(PlaybackPlan.empty)

    def logErrors(errors: NonEmptyChain[DomainError]): IO[Unit] =
      errors.traverse_(error => logger.error(error.toString))

    private def parseTracksWithLogs(paths: Seq[Path]): IO[Seq[ValidatedNec[DomainError, Track]]] =
      IO.pure(paths.map(path => path -> parser(path))).flatMap { parsed =>
        parsed.traverse_ {
          case (path, Valid(_)) =>
            logger.debug(s"Track parse OK: ${path.getFileName}")
          case (path, Invalid(errors)) =>
            logger.error(
              s"Track parse ERROR: ${path.getFileName} -> ${errors.toChain.toList.map(_.toString).mkString(" | ")}"
            )
        } *> IO.pure(parsed.map(_._2))
      }

    private def replacePlanIfChanged(plan: PlaybackPlan): IO[IorNec[DomainError, PlaybackPlan]] = {
      tracksRef.get.flatMap { current =>
        if current == plan then IO.pure(Ior.right(current))
        else playback.replace(plan, policy) *> tracksRef.set(plan) *> IO.pure(Ior.right(plan))
      }
    }

    private def maybeOverridePlan(
      plan: PlaybackPlan,
      trackSourceFiles: Seq[Path]
    ): IO[IorNec[DomainError, PlaybackPlan]] =
      musicFile match {
        case None => logger.debug(s"Music file not found") *> IO.pure(Ior.right(plan))
        case Some(path) =>
          IO.blocking(Files.exists(path)).flatMap {
            case false => IO.pure(Ior.right(plan))
            case true =>
              IO.blocking {
                TrackFileParser.compileAndEvaluateFile[Option[Seq[Track]]](
                  scalaFile = path.toString,
                  className = "Music",
                  methodName = "music",
                  sourceFiles = trackSourceFiles.map(_.toString)
                )
              }.flatMap {
                case Valid(Some(musicTracks)) if musicTracks.nonEmpty =>
                  IO.pure {
                    Tracks
                      .from(musicTracks)
                      .map(tracks => PlaybackPlan.fromTracks(tracks, timing))
                      .flatten
                  }
                case Valid(_)        => IO.pure(Ior.right(plan))
                case Invalid(errors) => IO.pure(Ior.both(errors, plan))
              }
          }
      }

    override def scanOnce(logErrorsOnly: Boolean = true): IO[PlaybackPlan] =
      trackFiles().flatMap { paths =>
        if (paths.isEmpty) logger.error(s"No track files found in $directory").as(PlaybackPlan.empty)
        else
          parseTracksWithLogs(paths)
            .map(loadTracks)
            .flatMap(iorPlan => iorPlan.map(plan => maybeOverridePlan(plan, paths)).sequenceIO.map(_.flatten))
            .flatMap { iorPlan =>
              if logErrorsOnly then IO.pure(iorPlan)
              else iorPlan.map(plan => replacePlanIfChanged(plan)).sequenceIO.map(_.flatten)
            }
            .flatMap {
              case Ior.Left(errors)       => logErrors(errors).as(PlaybackPlan.empty)
              case Ior.Right(plan)        => IO.pure(plan)
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
                stream.iterator().asScala
                  .filter(Files.isRegularFile(_))
                  .filter(isScalaFile)
                  .filterNot(isConfiguredMusicFile)
                  .toSeq
              )
            )
      }

    private def isConfiguredMusicFile(path: Path): Boolean =
      musicFile.exists(configured =>
        path.toAbsolutePath.normalize() == configured.toAbsolutePath.normalize()
      )

    def loadTracks(parsedTracks: Seq[ValidatedNec[DomainError, Track]]): IorNec[DomainError, PlaybackPlan] =
      val (parseErrors, validTracks) = parsedTracks.foldLeft((List.empty[DomainError], Vector.empty[Track])) {
        case ((errorsAcc, tracksAcc), Valid(track)) =>
          (errorsAcc, tracksAcc :+ track)
        case ((errorsAcc, tracksAcc), Invalid(errors)) =>
          (errorsAcc ++ errors.toChain.toList, tracksAcc)
      }

      val parseErrorsNec = NonEmptyChain.fromSeq(parseErrors)

      if validTracks.isEmpty then
        parseErrorsNec match
          case Some(errors) => Ior.left(errors)
          case None         => Ior.left(NonEmptyChain.one(DomainError.EmptyTracks))
      else {
        val compiled =
          Tracks
            .from(validTracks)
            .map(compiler)
            .flatten

        parseErrorsNec match
          case None => compiled
          case Some(errors) =>
            compiled match
              case Ior.Left(trackErrors)       => Ior.left(errors ++ trackErrors)
              case Ior.Right(plan)             => Ior.both(errors, plan)
              case Ior.Both(trackErrors, plan) => Ior.both(errors ++ trackErrors, plan)
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
          waitForChanges(watchService).foreverM
      }

    private def waitForChanges(watchService: WatchService): IO[Unit] =
      IO.interruptibleMany(watchService.take()).flatMap { key =>
        val shouldScan = requiresScan(key.pollEvents().asScala)
        IO.blocking(key.reset()).void *>
          (if (shouldScan) IO.sleep(pollInterval) *> discardPendingEvents(watchService) *> scanOnce().void else IO.unit)
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
