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

    private val stateRef: Ref[IO, Map[Path, Track]] = Ref.unsafe(Map.empty)
    private val runningRef: Ref[IO, Boolean]        = Ref.unsafe(false)

    override def scanOnce: IO[List[Track]] =
      for {
        trackFiles <- IO.blocking {
          if (!Files.exists(directory)) {
            List.empty[Path]
          } else {
            Files
              .list(directory)
              .iterator()
              .asScala
              .filter(path => Files.isRegularFile(path))
              .filter(path => {
                val name = path.getFileName.toString.toLowerCase
                name.endsWith(".scala")
              })
              .toList
          }
        }
        _ <- if (!Files.exists(directory)) logger.warn(s"Track directory does not exist: $directory") else IO.unit
        _ <- if (trackFiles.isEmpty) logger.info(s"No track files found in $directory") else IO.unit
        results <- trackFiles.foldLeft(IO.pure((List.empty[(Path, Track)], List.empty[Track]))) { (acc, path) =>
          for {
            previous <- acc
//            _        <- logger.info(s"Found track file: ${path.getFileName}")
//            _        <- logger.info(s"scanOnce: parsing ${path.toAbsolutePath}, lastModified=${Files.getLastModifiedTime(path)}")
//            _        <- IO.blocking(Files.readString(path)).flatMap(s => logger.info(s"scanOnce: file head=${s.take(500)}"))
            parsed <- IO.blocking {
              parser(path) match {
                case Valid(track) =>
                  // if false then Right(path -> track) else Left(s"track $track")
                  compiler(track) match {
                    case compiled if compiled.events.forall(_.isRight) =>
                      Right(path -> track)
                    case _ =>
                      Left(s"Track compilation failed for $path")
                  }
                case Invalid(errors) =>
                  Left(s"Track parse failed for $path: ${errors.toList.mkString(", ")}")
              }
            }
            next <- parsed match {
              case Right(item)   => IO.pure((item :: previous._1, item._2 :: previous._2))
              case Left(message) => logger.error(message).as(previous)
            }
          } yield next
        }
        (pathTracks, tracks) = results
        _ <- stateRef.set(pathTracks.toMap)
        _ <-
          if (tracks.nonEmpty) {
            logger.info(s"Loaded ${tracks.size} track(s) from $directory") *> playback.replace(
              tracks.reverse,
              timing,
              policy
            )
          } else {
            IO.unit
          }
      } yield tracks.reverse

    override def start: IO[Unit] =
      logger.info(s"Starting track monitor for $directory (poll interval: $pollInterval)") *>
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
                logger.info(s"Watch service registered for $directory")

                while (runningRef.get.unsafeRunSync()) {
                  val key = watchService.take()
                  val shouldScan =
                    if (requiresScan(key.pollEvents().asScala)) {
                      key.reset()
                      Thread.sleep(pollInterval.toMillis)

                      var pendingKey = watchService.poll()
                      while (pendingKey != null) {
                        pendingKey.pollEvents()
                        pendingKey.reset()
                        pendingKey = watchService.poll()
                      }
                      true
                    } else {
                      key.reset()
                      false
                    }

                  if (shouldScan) {
                    scanOnce.unsafeRunSync()
                  }
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
