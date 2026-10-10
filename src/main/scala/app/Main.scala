package app

import app.config.Environment
import app.console.PlaybackCommandConsole
import app.midi.ReactiveSynth
import app.midi.toMidiMessages
import app.playback.PlaybackController
import app.playback.RepeatPolicy
import app.playback.TrackDirectoryMonitor
import cats.data.EitherT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource

import java.nio.file.Paths
import scala.concurrent.duration._

object Main extends IOApp {

  private def asRuntimeError(error: app.domain.DomainError): RuntimeException =
    new RuntimeException(error.toString)

  private def loadConfigSource(args: List[String]): ConfigSource =
    args.headOption.map(ConfigSource.file).getOrElse(ConfigSource.default)

  private def liftDomainError[A](result: IO[Either[app.domain.DomainError, A]]): IO[A] =
    result.flatMap {
      case Left(err)  => IO.raiseError(asRuntimeError(err))
      case Right(res) => IO.pure(res)
    }

  private def monitorProgram(
    env: Environment,
    sendEvent: app.domain.AbsoluteMidiEvent => IO[Unit],
    configSource: ConfigSource,
    logger: Logger[IO]
  ): IO[Unit] =
    for {
      controller <- PlaybackController.live(sendEvent)
      monitor <- TrackDirectoryMonitor.live(
        directory = Paths.get(env.pathsConfig.tracks),
        playback = controller,
        timing = env.timingContext,
        musicFile = Paths.get(env.pathsConfig.musicFile),
        policy = RepeatPolicy.forever,
        pollInterval = env.pathsConfig.pollingInterval.millis
      )
      _ <-
        if env.startPlaybackOnStartup then
          monitor.scanOnce *> monitor.start *> logger.debug("Startup playback is enabled. Playback started.")
        else logger.debug("Startup playback is disabled. Waiting for console command.")
      _ <- PlaybackCommandConsole.run(env, monitor, controller, configSource)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    val logger       = Slf4jLogger.getLogger[IO]
    val configSource = loadConfigSource(args)

    val program =
      for {
        env <- IO.fromEither(Environment.load(source = configSource).left.map(asRuntimeError))
        _ <- liftDomainError(
          ReactiveSynth
            .outputResource[IO](env.midiOutputConfig)
            .use { sendMidi =>
              val sendEvent = (event: app.domain.AbsoluteMidiEvent) =>
                liftDomainError(sendMidi(event.command.toMidiMessages).value).void
              EitherT.liftF(monitorProgram(env, sendEvent, configSource, logger))
            }
            .value
        )
      } yield ExitCode.Success

    program.handleErrorWith { error =>
      logger.error(error)(s"Main failed: ${error.getMessage}") *> IO.pure(ExitCode.Error)
    }
  }
}
