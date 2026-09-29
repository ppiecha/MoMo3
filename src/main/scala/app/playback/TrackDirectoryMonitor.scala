package app.playback

import app.domain.DomainError.{MusicFileParseFailed, ValidationFailed}
import app.domain.{
  AbsoluteMidiEvent,
  DomainError,
  PlaybackPlan,
  TimingContext,
  Track,
  Tracks,
  ValidationError,
  validationToDomainError
}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, ValidatedNec}
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
  def scanOnce(logErrorsOnly: Boolean = true): IO[PlaybackPlan]
  def start: IO[Unit]
  def stop: IO[Unit]
}

object TrackDirectoryMonitor {

  type TrackParser = Path => ValidatedNec[DomainError, Track]

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
    compiler: Tracks => ValidatedNec[ValidationError, PlaybackPlan],
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
    parser: TrackParser,
    compiler: Tracks => ValidatedNec[ValidationError, PlaybackPlan],
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

    private def mapMusicFileErrors[A](path: Path, errors: NonEmptyChain[A]): NonEmptyChain[DomainError] =
      errors.map(error => MusicFileParseFailed(s"Music file parse failed for $path: $error"))

    private def logMusicFileErrors[A](path: Path, errors: NonEmptyChain[A]): IO[Unit] =
      logErrors(mapMusicFileErrors(path, errors))

    private def replacePlanIfChanged(plan: PlaybackPlan): IO[Unit] = {
      tracksRef.get.flatMap { current =>
        if (current == plan) IO.unit
        else playback.replace(plan, policy) *> tracksRef.set(plan)
      }
    }

    private def maybeOverridePlan(plan: PlaybackPlan): IO[PlaybackPlan] =
      musicFile match {
        case None => logger.debug(s"Music file not found") *> IO.pure(plan)
        case Some(path) =>
          IO.blocking(Files.exists(path)).flatMap {
            case false => IO.pure(plan)
            case true =>
              IO.blocking {
                TrackFileParser.compileAndEvaluateFile[Option[Seq[Track]]](
                  scalaFile = path.toString,
                  className = "Music",
                  methodName = "music"
                )
              }.flatMap {
                case Valid(Some(musicTracks)) if musicTracks.nonEmpty =>
                  Tracks.from(musicTracks).andThen(tracks => PlaybackPlan.fromTracks(tracks, timing)) match {
                    case Valid(playbackPlan) =>
                      logger.debug(s"Using ${musicTracks.size} track(s) from music file: $path") *>
                        IO.pure(playbackPlan)
                    case Invalid(errors) =>
                      logMusicFileErrors(path, errors) *>
                        IO.pure(plan)
                  }
                case Valid(_) => IO.pure(plan)
                case Invalid(errors) =>
                  logMusicFileErrors(path, errors) *>
                    IO.pure(plan)
              }
          }
      }

    override def scanOnce(logErrorsOnly: Boolean = true): IO[PlaybackPlan] =
      trackFiles().flatMap { paths =>
        if (paths.isEmpty) logger.error(s"No track files found in $directory").as(PlaybackPlan.empty)
        else
          (loadTracks(paths) match {
            case Invalid(errors) =>
              logErrors(errors) *>
                IO.pure(PlaybackPlan.empty)
            case Valid(plan) =>
              maybeOverridePlan(plan)
          })
            .flatMap { plan =>
              plan.isEmpty match {
                case true => logger.error(s"Playback plan is empty after scanning $directory").as(PlaybackPlan.empty)
                case false =>
                  if logErrorsOnly then IO.pure(plan)
                  else replacePlanIfChanged(plan) *> IO.pure(plan)
              }
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
              IO.blocking(stream.iterator().asScala.filter(Files.isRegularFile(_)).filter(isScalaFile).toSeq)
            )
      }

//    private def loadTrack(path: Path): IO[ValidatedNec[DomainError, CompiledTrack]] =
//      IO.blocking {
//        parser(path) match {
//          case Valid(track) => Valid(compiler(track))
//          case Invalid(errors) => Invalid(errors)
//        }
//      }

    def loadTracks(paths: Seq[Path]): ValidatedNec[DomainError, PlaybackPlan] =
      paths
        .map(parser)
        .sequence
        .map { tracks =>
          Tracks.from(tracks).leftMap(validationToDomainError) match {
            case Valid(tracks)   => compiler(tracks).leftMap(validationToDomainError)
            case Invalid(errors) => errors.invalid[PlaybackPlan]
          }
        }
        .andThen(identity)

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
