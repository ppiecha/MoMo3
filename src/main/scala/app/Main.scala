package app

import app.config.Environment
import app.midi.ReactiveSynth
import app.midi.toMidiMessages
import app.playback.PlaybackController
import app.playback.RepeatPolicy
import app.playback.TrackDirectoryMonitor
import cats.data.EitherT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.Paths
import scala.concurrent.duration._

object Main extends IOApp {

  private def asRuntimeError(error: app.domain.DomainError): RuntimeException =
    new RuntimeException(error.toString)

  private def liftDomainError[A](result: IO[Either[app.domain.DomainError, A]]): IO[A] =
    result.flatMap {
      case Left(err)  => IO.raiseError(asRuntimeError(err))
      case Right(res) => IO.pure(res)
    }

  private def monitorProgram(env: Environment, sendEvent: app.domain.AbsoluteMidiEvent => IO[Unit]): IO[Unit] =
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
      _ <- monitor.scanOnce
      _ <- monitor.start
      _ <- IO.never
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    val logger = Slf4jLogger.getLogger[IO]

    val program =
      for {
        env <- IO.fromEither(Environment.load().left.map(asRuntimeError))
        _ <- liftDomainError(ReactiveSynth.outputResource[IO](env.midiOutputConfig).use { sendMidi =>
          val sendEvent = (event: app.domain.AbsoluteMidiEvent) =>
            liftDomainError(sendMidi(event.command.toMidiMessages).value).void
          EitherT.liftF(monitorProgram(env, sendEvent))
        }.value)
      } yield ExitCode.Success

    program.handleErrorWith { error =>
      logger.error(error)(s"Main failed: ${error.getMessage}") *> IO.pure(ExitCode.Error)
    }
  }
}
