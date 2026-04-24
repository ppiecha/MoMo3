package app.demo

import cats.effect._

import app.config.*
import app.midi.*
import app.application.*
import app.domain.Track
import app.application.TrackCompiler

// logger to env
// fluidsynth to env
// on start kill the process if running and start it
// (copilot) is there anything else what can be improved in project
// play in a loop and update tracks online
// modify main to print/play list of tracks stopping after longest track
// test multiple tracks
// complete readme

import Tracks.*
import app.domain.*

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
      val env          = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
      PlaybackService.play[IO](List(track4).map(track => TrackCompiler.compile(track, env)), env)
  }
}