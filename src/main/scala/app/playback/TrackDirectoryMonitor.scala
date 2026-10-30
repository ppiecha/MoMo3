package app.playback

import app.domain.DomainError
import app.domain.PlaybackPlan
import app.domain.TimingContext
import app.domain.Track
import app.domain.Tracks
import app.playback.TrackFileParser.classNameFromFilePath
import app.syntax.flatten
import cats.data.{Ior, IorNec, NonEmptyChain, NonEmptyList, ValidatedNec}
import cats.effect.FiberIO
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.nio.file.WatchEvent
import java.nio.file.WatchService
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Watches a directory containing Scala track definitions. A new file, a deletion, or a modification triggers a full
  * scan and a rebuild of the active playback.
  */
trait TrackDirectoryMonitor {
  protected def scanOnce: IO[IorNec[DomainError, PlaybackPlan]]
  protected def start: IO[Unit]
  def stop: IO[Unit]
  def scanReplaceAndResume: IO[PlaybackPlan]
  def scanAndPlay: IO[PlaybackPlan]
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
    policy: RepeatPolicy,
    pollInterval: FiniteDuration
  ): IO[TrackDirectoryMonitor] = {
    for {
      watcherRef <- Ref.of[IO, Option[FiberIO[Unit]]](None)
      tracksRef  <- Ref.of[IO, PlaybackPlan](PlaybackPlan.EmptyPlaybackPlan)
    } yield new FileSystemTrackDirectoryMonitor(
      directory,
      playback,
      timing,
      musicFile,
      policy,
      pollInterval,
      watcherRef,
      tracksRef
    )
  }

  private final class FileSystemTrackDirectoryMonitor(
    directory: Path,
    playback: PlaybackController,
    timing: TimingContext,
    musicFile: Path,
    policy: RepeatPolicy,
    pollInterval: FiniteDuration,
    watcherRef: Ref[IO, Option[FiberIO[Unit]]],
    tracksRef: Ref[IO, PlaybackPlan]
  ) extends TrackDirectoryMonitor {

    val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    
    private def logErrors(errors: NonEmptyChain[DomainError]): IO[Unit] =
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
            TrackFileParser.compileAndEvaluateFile[ValidatedNec[String, List[Track]]](
              scalaFile = musicFile.toString,
              className = classNameFromFilePath(musicFile.getFileName),
              methodName = "playWrapper",
              sourceFiles = trackSourceFiles.map(_.toString),
              classpath = TrackFileParser.resolvedClasspath()
            )
          }.map(
            _.fold(
              musicErrors => Ior.left(musicErrors),
              musicTracks =>
                Tracks
                  .from(musicTracks)
                  .leftMap(_.map {
                    case mismatch: DomainError.DurationMismatch => mismatch.copy(source = Some(musicFile.toString))
                    case other                                  => other
                  })
                  .map(tracks => PlaybackPlan.fromTracks(tracks)(using timing))
                  .flatten
            )
          )
      }

    private def loadCurrentPlan: IO[IorNec[DomainError, PlaybackPlan]] =
      trackFiles().flatMap(loadPlanFromMusic)

    override def scanOnce: IO[IorNec[DomainError, PlaybackPlan]] = {
      stop *>                               // Stop the watcher before scanning to avoid concurrent modifications
        loadCurrentPlan.flatTap(_ => start) // Start the watcher after scanning
    }

    private def replaceWithScanResult(result: IorNec[DomainError, PlaybackPlan]): IO[PlaybackPlan] =
      result match {
        case Ior.Left(errors)       => logErrors(errors) *> playback.activePlan.map(_.getOrElse(PlaybackPlan.EmptyPlaybackPlan))
        case Ior.Right(plan)        => replacePlanIfChanged(plan)
        case Ior.Both(errors, plan) => logErrors(errors) *> replacePlanIfChanged(plan)
      }

    override def scanReplaceAndResume: IO[PlaybackPlan] =
      scanOnce.flatMap(replaceWithScanResult)

    override def scanAndPlay: IO[PlaybackPlan] =
      scanOnce.flatMap {
        case Ior.Left(errors)       => logErrors(errors) *> playback.activePlan.map(_.getOrElse(PlaybackPlan.EmptyPlaybackPlan))
        case Ior.Right(plan)        => playback.play(plan, policy) *> IO.pure(plan)
        case Ior.Both(errors, plan) => logErrors(errors) *> playback.play(plan, policy) *> IO.pure(plan)
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
        IO.blocking(key.reset()).void *> runFullRescanIfNeeded(shouldScan, watchService)
      }

    private def runFullRescanIfNeeded(shouldScan: Boolean, watchService: WatchService): IO[Unit] =
      if shouldScan then
        IO.sleep(pollInterval) *>
          discardPendingEvents(watchService) *>
          logger.debug(s"Detected changes in $directory, rescanning...") *>
          loadCurrentPlan
            .flatMap(replaceWithScanResult)
            .flatTap(_ => logger.debug(s"Rescan completed for $directory"))
            .void
      else IO.unit

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
