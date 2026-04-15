package app.demo

import cats.effect._
import fs2.Stream
import javax.sound.midi._

import app.config.*
import app.midi.*
import app.application.*
import app.domain.Track

import cats.syntax.all.*

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.DurationInt
import app.application.TrackCompiler

// modify main to print/play list of tracks stopping after longest track
// test multiple tracks
// complete readme

object Main extends IOApp.Simple {

  private def configureLogging(): Unit = {
    System.setProperty("org.slf4j.simpleLogger.showDateTime", "true")
    System.setProperty("org.slf4j.simpleLogger.dateTimeFormat", "HH:mm:ss.SSS")
  }

  def run: IO[Unit] = {
    import Tracks.*
    configureLogging()
    Slf4jLogger.create[IO].flatMap { logger =>
      given Logger[IO] = logger
      val env          = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
      PlaybackService.play[IO](List(track4).map(track => TrackCompiler.compile(track, env)), env)
    }
  }
}
