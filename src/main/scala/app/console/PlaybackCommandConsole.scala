package app.console

import app.config.ConsoleInput
import app.config.Environment
import app.playback.PlaybackController
import app.playback.PlaybackStatus
import app.playback.RepeatPolicy
import app.playback.TrackDirectoryMonitor
import cats.effect.IO
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource

import java.nio.file.Paths
import scala.concurrent.duration._

object PlaybackCommandConsole {

  val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]

  private final case class Session(
    env: Environment,
    monitor: TrackDirectoryMonitor
  )

  private def printMenu: IO[Unit] =
    logger.debug(
      "(p) pause/play, (s) start/stop, (r) restart playback, (n) reload config, (c) force scan, (q) quit"
//      """
//        |Available commands:
//        |r - reload (stop and start)
//        |s - start if stopped, stop if playing
//        |p - pause if playing, play if paused
//        |n - reload config without restarting process
//        |c - force scan (run scanOnce)
//        |q - quit
//        |""".stripMargin
    )

  private def readCommand(input: ConsoleInput): IO[Char] =
    IO.blocking(input.readKey()).map(_.toLower)

  private def createMonitor(env: Environment, controller: PlaybackController): IO[TrackDirectoryMonitor] =
    TrackDirectoryMonitor.live(
      directory = Paths.get(env.pathsConfig.tracks),
      playback = controller,
      timing = env.timingContext,
      musicFile = Paths.get(env.pathsConfig.musicFile),
      policy = RepeatPolicy.forever,
      pollInterval = env.pathsConfig.pollingInterval.millis
    )

  private def reloadEnvironment(configSource: ConfigSource, input: ConsoleInput): IO[Either[String, Environment]] =
    IO.pure(Environment.load(source = configSource, input = input).left.map(_.toString))

  def run(
    initialEnv: Environment,
    initialMonitor: TrackDirectoryMonitor,
    controller: PlaybackController,
    configSource: ConfigSource
  ): IO[Unit] = {
    def loop(session: Session): IO[Unit] =
      printMenu *>
        readCommand(session.env.input).flatMap {
          case 'r' =>
            for {
              _ <- controller.stop
              _ <- session.monitor.stop
              _ <- session.monitor.scanOnce
              _ <- session.monitor.start
              _ <- logger.debug("Playback reloaded: stopped and started.")
              _ <- loop(session)
            } yield ()

          case 's' =>
            controller.status.flatMap {
              case PlaybackStatus.Stopped =>
                for {
                  _ <- session.monitor.scanOnce
                  _ <- session.monitor.start
                  _ <- logger.debug("Playback started.")
                  _ <- loop(session)
                } yield ()

              case PlaybackStatus.Playing =>
                for {
                  _ <- controller.stop
                  _ <- session.monitor.stop
                  _ <- logger.debug("Playback stopped.")
                  _ <- loop(session)
                } yield ()

              case PlaybackStatus.Paused =>
                for {
                  _ <- controller.stop
                  _ <- session.monitor.stop
                  _ <- logger.debug("Playback stopped.")
                  _ <- loop(session)
                } yield ()
            }

          case 'p' =>
            controller.status.flatMap {
              case PlaybackStatus.Playing =>
                controller.pause *> logger.debug("Playback paused.") *> loop(session)
              case PlaybackStatus.Paused =>
                controller.resume *> logger.debug("Playback resumed.") *> loop(session)
              case PlaybackStatus.Stopped =>
                logger.debug("Playback is stopped. Use 's' to start.") *> loop(session)
            }

          case 'n' =>
            controller.status.flatMap { previousStatus =>
              reloadEnvironment(configSource, session.env.input).flatMap {
                case Left(loadError) =>
                  logger.error(s"Configuration reload failed: $loadError") *> loop(session)

                case Right(reloadedEnv) =>
                  for {
                    _          <- controller.stop
                    _          <- session.monitor.stop
                    newMonitor <- createMonitor(reloadedEnv, controller)
                    _ <- previousStatus match {
                      case PlaybackStatus.Stopped =>
                        logger.debug("Configuration reloaded. Playback remains stopped.")
                      case PlaybackStatus.Playing =>
                        newMonitor.scanOnce *> newMonitor.start *> logger.debug(
                          "Configuration reloaded and playback restarted."
                        )
                      case PlaybackStatus.Paused =>
                        newMonitor.scanOnce *> newMonitor.start *> controller.pause *> logger.debug(
                          "Configuration reloaded and playback restored in paused mode."
                        )
                    }
                    _ <- loop(session.copy(env = reloadedEnv, monitor = newMonitor))
                  } yield ()
              }
            }

          case 'c' =>
            session.monitor.scanOnce *> logger.debug("Force scan completed.") *> loop(session)

          case 'q' =>
            controller.stop *> session.monitor.stop *> logger.debug("Quitting application.")

          case '\r' | '\n' =>
            loop(session)

          case key =>
            logger.error(s"Unknown command '$key'. Please use one of: r, s, p, n, c, q.") *> loop(session)
        }

    loop(Session(initialEnv, initialMonitor))
  }
}
