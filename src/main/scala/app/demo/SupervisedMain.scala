package app.demo

import app.config.*
import app.domain.*
import app.midi.{ReactiveSynth, toMidiMessages}
import app.tracks.TrackFolderSupervisor
import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import java.nio.file.Paths
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object SupervisedMain extends IOApp.Simple {

  def run: IO[Unit] = {
    given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    val trackFolder          = Paths.get("tracks")

    Environment
      .from(bpm = 120)
      .fold(
        err => logger.error(s"Invalid environment: $err"),
        env =>
          ReactiveSynth
            .outputResource[IO](env.midiOutputConfig)
            .use { sendMidi =>
              val send: AbsoluteMidiEvent => IO[Unit] = event =>
                sendMidi(event.command.toMidiMessages).value.void
              EitherT.liftF[IO, DomainError, Unit](
                TrackFolderSupervisor.run[IO](trackFolder, env, send)
              )
            }
            .value
            .flatMap {
              case Left(err) => logger.error(s"Playback error: $err")
              case Right(_)  => IO.unit
            }
      )
  }
}
