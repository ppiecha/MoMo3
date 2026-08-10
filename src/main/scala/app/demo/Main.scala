package app.demo

import cats.effect.*
import app.config.*
import app.midi.*
import app.application.*
import app.application.TrackCompiler
import cats.syntax.all.*
import cats.data.EitherT
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

// complete readme - add
// on start kill the process if running and start it
// play in a loop and update tracks online
// test multiple tracks
// kolejnosc eventow w tym samym czasie
// program change -3 event dopiero podczas translacji do javax.sound.midi

import Tracks.*
import app.domain.*

object Main extends IOApp.Simple {

  def program: IO[Either[DomainError, Unit]] = {

    val pipeline = for {
      env <- EitherT.fromEither[IO](
        Environment
          .from(bpm = 60)
          .toEither
          .leftMap(errors => ValidationError.InvalidConfig(errors.toNonEmptyList))
          .leftMap(DomainError.ValidationFailed.apply)
      )

      //      _ <- EitherT(
      //        ReactiveSynth
      //          .outputResource[IO](env.midiOutputConfig)
      //          .use { send =>
      //            PlaybackService
      //              .play(
      //                List(TrackCompiler.compile(track4, env.timingContext)),
      //                env.timingContext,
      //                event => send(event.command.toMidiMessages)
      //              )
      //          }
      //          .attempt
      //          .map(_.leftMap(e => DomainError.PlaybackFailed(e.getMessage)))
      //      )
      _ <- ReactiveSynth
        .outputResource[IO](env.midiOutputConfig)
        .use { send =>
          PlaybackService
            .play(
              List(TrackCompiler.compile(track4, env.timingContext)),
              env.timingContext,
              event => send(event.command.toMidiMessages)
            )
        }

    } yield ()

    pipeline.value
  }

  def run: IO[Unit] = {
    val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    program.flatTap {
      case Left(err) => logger.error(s"Playback failed: $err")
      case Right(_)  => logger.info("Playback finished")
    }.void
  }

}
