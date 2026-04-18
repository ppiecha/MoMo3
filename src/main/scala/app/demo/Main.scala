package app.demo

import cats.effect._

import app.config.*
import app.midi.*
import app.application.*
import app.domain.Track
import app.application.TrackCompiler

// logger to env
// modify main to print/play list of tracks stopping after longest track
// test multiple tracks
// complete readme

import Tracks.*

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
      val env          = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
      PlaybackService.play[IO](List(track4).map(track => TrackCompiler.compile(track, env)), env)
  }
}