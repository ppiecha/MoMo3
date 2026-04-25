package app.demo

import cats.effect._

import app.config.*
import app.midi.*
import app.application.*
import app.domain.Track
import app.application.TrackCompiler

// logger to env
// fluidsynth to env
// split env
// on start kill the process if running and start it
// play in a loop and update tracks online
// test multiple tracks
// complete readme

import Tracks.*
import app.domain.*

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    val env = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
    val compiledTracks = List(TrackCompiler.compile(track4, env))
    ReactiveSynth.outputResource[IO](env).use { send =>
      PlaybackService.play(compiledTracks, env, event => send(event.command.toMidiMessages))
    }
  }
}
