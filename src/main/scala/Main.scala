package app

import app.config.Environment
import app.domain.PlaybackPlan
import app.midi.ReactiveSynth
import app.midi.toMidiMessages
import app.playback.{PlaybackController, RepeatPolicy, TrackDirectoryMonitor, TrackFileParser}
import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.util.Random // nieużywany

import java.nio.file.Paths
import scala.concurrent.duration.*

object Main extends IOApp {

  private val DemoDirName = "demo-tracks"

  override def run(args: List[String]): IO[ExitCode] = {
    val logger = Slf4jLogger.getLogger[IO]

    def liftDomainError[A](result: IO[Either[app.domain.DomainError, A]]): IO[A] =
      result.flatMap {
        case Left(err)  => IO.raiseError(new RuntimeException(err.toString))
        case Right(res) => IO.pure(res)
      }

    val program =
      for {
        env <- IO.fromEither(Environment.load().left.map(err => new RuntimeException(err.toString)))
        _ <- liftDomainError(
          ReactiveSynth
            .outputResource[IO](env.midiOutputConfig)
            .use { sendMidi =>
              val sendEvent = (event: app.domain.AbsoluteMidiEvent) =>
                liftDomainError(sendMidi(event.command.toMidiMessages).value).void
              val controller = PlaybackController.live(sendEvent)
              val monitor = TrackDirectoryMonitor.live(
                directory = Paths.get(env.pathsConfig.tracks),
                parser = TrackFileParser.compileAndEvaluateFile,
                compiler = tracks => PlaybackPlan.fromTracks(tracks, env.timingContext),
                playback = controller,
                timing = env.timingContext,
                musicFile = Some(Paths.get(env.pathsConfig.musicFile)),
                policy = RepeatPolicy.forever,
                pollInterval = env.pathsConfig.pollingInterval.millis
              )

              EitherT.liftF(
                for {
                  _ <- monitor.scanOnce() // TODO logerrorsonly
                  _ <- monitor.start
                  _ <- IO.never
                } yield ()
              )
            }
            .value
        )
      } yield ExitCode.Success

    program.handleErrorWith { error =>
      logger.error(error)(s"Main failed: ${error.getMessage}") *> IO.pure(ExitCode.Error)
    }
  }
}
