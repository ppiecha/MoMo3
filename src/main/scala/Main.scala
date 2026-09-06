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
    val directoryPath = args.headOption
      .map(Paths.get(_))
      .getOrElse(Paths.get(DemoDirName).toAbsolutePath.normalize())

    val program =
      for {
        env <- IO.fromEither(Environment.from(bpm = 60).left.map(err => new RuntimeException(err.toString)))
        _ <- ReactiveSynth
          .outputResource[IO](env.midiOutputConfig)
          .use { sendMidi =>
            val sendEvent = (event: app.domain.AbsoluteMidiEvent) =>
              sendMidi(event.command.toMidiMessages).value.flatMap {
                case Left(err) => IO.raiseError(new RuntimeException(err.toString))
                case Right(()) => IO.unit
              }
            val controller = PlaybackController.live(sendEvent, logger)
            val monitor = TrackDirectoryMonitor.live(
              directory = directoryPath,
              parser = TrackFileCompiler.compileAndEvaluateFile,
              compiler = track => TrackCompiler.compile(track, env.timingContext),
              playback = controller,
              timing = env.timingContext,
              policy = RepeatPolicy.forever,
              logger = logger,
              pollInterval = 300.millis
            )

            cats.data.EitherT.right[app.domain.DomainError] {
              for {
                loaded <- monitor.scanOnce
                _      <- logger.info(s"Demo directory ready at: ${directoryPath.toAbsolutePath}")
                _      <- logger.info(s"Loaded ${loaded.size} track(s). Monitoring for changes...")
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

  private def writeIfMissing(path: Path, content: String): Unit =
    if (!Files.exists(path)) {
      Files.writeString(path, content)
    }
}
