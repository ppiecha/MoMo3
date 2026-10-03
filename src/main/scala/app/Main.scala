package app

import app.config.Environment
import app.domain.PlaybackPlan
import app.midi.ReactiveSynth
import app.midi.toMidiMessages
import app.playback.PlaybackController
import app.playback.RepeatPolicy
import app.playback.TrackDirectoryMonitor
import app.playback.TrackFileParser
import cats.data.EitherT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.Paths
import scala.concurrent.duration._

object Main extends IOApp {

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
                  _ <- monitor.scanOnce(false) // TODO logerrorsonly
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
