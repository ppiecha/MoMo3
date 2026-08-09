package app.demo

import cats.effect.*
import app.config.*
import app.midi.*
import app.application.*
import app.application.TrackCompiler
import cats.syntax.all.*
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

  def program: IO[Either[ValidationError, Unit]] = {
    Environment
      .from(bpm = 60)
      .toEither
      .leftMap(errors => ValidationError.InvalidConfig(errors.toNonEmptyList))
      .traverse { env =>
        val compiledTracks = List(TrackCompiler.compile(track4, env.timingContext))
        ReactiveSynth.outputResource[IO](env.midiOutputConfig).use { send =>
          PlaybackService.play(
            compiledTracks,
            env.timingContext,
            event => send(event.command.toMidiMessages)
          )
        }
      }
  }

  def run: IO[Unit] = {
    val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    program.flatTap {
      case Left(err) => logger.error(s"Playback failed: $err")
      case Right(_) => logger.info("Playback finished")
    }.void
  }

}
