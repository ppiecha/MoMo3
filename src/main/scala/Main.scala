package app

import app.config.Environment
import app.midi.ReactiveSynth
import app.midi.toMidiMessages
import app.playback.{PlaybackController, RepeatPolicy, TrackCompiler, TrackDirectoryMonitor, TrackFileCompiler}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration.*

object Main extends IOApp {

  private val DemoDirName = "demo-tracks"

  override def run(args: List[String]): IO[ExitCode] = {
    val logger = Slf4jLogger.getLogger[IO]

    val program =
      for {
        env <- IO.fromEither(Environment.load().left.map(err => new RuntimeException(err.toString)))
        _ <- ReactiveSynth
          .outputResource[IO](env.midiOutputConfig)
          .use { sendMidi =>
            val sendEvent = (event: app.domain.AbsoluteMidiEvent) =>
              sendMidi(event.command.toMidiMessages).value.flatMap {
                case Left(err) => IO.raiseError(new RuntimeException(err.toString))
                case Right(()) => IO.unit
              }
            val controller = PlaybackController.live(sendEvent)
            val monitor = TrackDirectoryMonitor.live(
              directory = Paths.get(env.pathsConfig.tracks),
              parser = TrackFileCompiler.compileAndEvaluateFile,
              compiler = track => TrackCompiler.compile(track, env.timingContext),
              playback = controller,
              timing = env.timingContext,
              musicFile = Some(Paths.get(env.pathsConfig.musicFile)),
              policy = RepeatPolicy.forever,
              pollInterval = env.pathsConfig.pollingInterval.millis
            )

            cats.data.EitherT.right[app.domain.DomainError] {
              for {
                loaded <- monitor.scanOnce
                _      <- monitor.start
                _      <- IO.never
              } yield ()
            }
          }
          .value
          .flatMap {
            case Left(err) => IO.raiseError(new RuntimeException(err.toString))
            case Right(()) => IO.unit
          }
      } yield ExitCode.Success

    program.handleErrorWith { error =>
      logger.error(error)(s"Main failed: ${error.getMessage}") *> IO.pure(ExitCode.Error)
    }
  }
}
