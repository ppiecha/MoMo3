package app.playback

import app.domain.{CompiledTrack, TimingContext, Track}
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.{Files, Path, StandardWatchEventKinds, WatchEvent, WatchKey}
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

  def live(
    directory: Path,
    parser: TrackParser,
    compiler: Track => CompiledTrack,
    playback: PlaybackController,
    timing: TimingContext,
    policy: PlaybackRepeatPolicy = PlaybackRepeatPolicy.none,
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
    policy: PlaybackRepeatPolicy,
    logger: Logger[IO],
    pollInterval: FiniteDuration
  ) extends TrackDirectoryMonitor {

    private val stateRef: Ref[IO, Map[Path, Track]] = Ref.unsafe(Map.empty)
    private val runningRef: Ref[IO, Boolean]        = Ref.unsafe(false)

    override def scanOnce: IO[List[Track]] =
      IO.blocking {
        if (!Files.exists(directory)) {
          logger.warn(s"Track directory does not exist: $directory")
          (List.empty[(Path, Track)], List.empty[Track])
        } else {
          Files
            .list(directory)
            .iterator()
            .asScala
            .filter(path => path.getFileName.toString.endsWith(".scala"))
            .toList
            .foldLeft((List.empty[(Path, Track)], List.empty[Track])) { case ((pathTracks, tracks), path) =>
              parser(path) match {
                case Valid(track) =>
                  compiler(track) match {
                    case compiled if compiled.events.forall(_.isRight) =>
                      val nextAccum = (path -> track) :: pathTracks
                      (nextAccum, track :: tracks)
                    case _ =>
                      logger.error(s"Track compilation failed for $path")
                      (pathTracks, tracks)
                  }
                case Invalid(errors) =>
                  logger.error(s"Track parse failed for $path: ${errors.toList.mkString(", ")}")
                  (pathTracks, tracks)
              }
            }
        }
      }.flatMap { case (pathTracks, tracks) =>
        stateRef.set(pathTracks.toMap) *> {
          if (tracks.nonEmpty) {
            logger.info(s"Loaded ${tracks.size} track(s) from $directory") *>
              playback.replace(tracks.reverse, timing, policy)
          } else {
            IO.unit
          }
        } *> IO.pure(tracks.reverse)
      }

    override def start: IO[Unit] =
      runningRef.set(true) *>
        IO.async_[Unit] { callback =>
          val watcherThread = new Thread(
            () => {
              try {
                val watchService = directory.getFileSystem.newWatchService()
                directory.register(
                  watchService,
                  StandardWatchEventKinds.ENTRY_CREATE,
                  StandardWatchEventKinds.ENTRY_MODIFY,
                  StandardWatchEventKinds.ENTRY_DELETE
                )

                while (runningRef.get.unsafeRunSync()) {
                  val key: WatchKey               = watchService.take()
                  val events: List[WatchEvent[_]] = key.pollEvents().asScala.toList
                  if (events.nonEmpty) {
                    scanOnce.unsafeRunSync()
                  }
                  key.reset()
                  Thread.sleep(pollInterval.toMillis)
                }

                callback(Right(()))
              } catch {
                case _: InterruptedException => callback(Right(()))
                case e: Throwable            => callback(Left(e))
              }
            },
            "track-directory-monitor"
          )
          watcherThread.setDaemon(true)
          watcherThread.start()
        }

    override def stop: IO[Unit] =
      runningRef.set(false) *> logger.info(s"Track monitor stopped for $directory")
  }
}
