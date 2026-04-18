package app.domain

import munit.CatsEffectSuite
import app.midi.*
import app.application.*
import javax.sound.midi.ShortMessage
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import cats.effect.*
import cats.effect.testkit.TestControl
import fs2.*
import scala.concurrent.duration.*
import TestTracks.*

class TrackSpec extends CatsEffectSuite {

  private def gaps[A](s: Stream[IO, A]): IO[List[FiniteDuration]] =
    s.evalMap(a => IO.monotonic.map(t => (a, t)))
      .compile
      .toList
      .map { xs =>
        xs.map(_._2).sliding(2).collect { case List(a, b) => b - a }.toList
      }

  private def times[A](s: Stream[IO, A]): IO[List[FiniteDuration]] =
    s.evalMap(a => IO.monotonic.map(t => (a, t)))
      .compile
      .toList
      .map { xs => xs.map(_._2) }

  test("One note track midi stream should produce NoteOn and NoteOff message") {
    PlaybackService.compiledTrackToStream[IO](TrackCompiler.compile(oneNoteTrack, testEnv)).compile.toList.flatMap {
      _.head.compile.toList.map { messages =>
        assertEquals(messages.size, 2)
        val List(noteOn, noteOff) = messages.map(_.asInstanceOf[ShortMessage])
        assertEquals(noteOn.getCommand, NOTE_ON)
        assertEquals(noteOn.getData1, 60)
        assertEquals(noteOff.getCommand, NOTE_OFF)
        assertEquals(noteOff.getData1, 60)
      }
    }
  }

  test("One note track midi event list should produce NoteOn and NoteOff message with proper ticks") {
    val events = TrackCompiler.compile(oneNoteTrack, testEnv).listOfMidiEvents.toList
    assertEquals(events.size, 2)
    val List(noteOn, noteOff) = events
    noteOn match {
      case Left(error) => fail(s"Expected first event to be a NoteMessage, but got error: $error")
      case Right(noteOn)   => {
        assertEquals(noteOn.getShortMessage.getCommand(), ShortMessage.NOTE_ON)
        assertEquals(noteOn.getTick, 0L)
      }
    }
    noteOff match {
      case Left(error) => fail(s"Expected second event to be a NoteMessage, but got error: $error")
      case Right(noteOff)   => {
        assertEquals(noteOff.getShortMessage.getCommand(), ShortMessage.NOTE_OFF)
        assertEquals(noteOff.getTick, 480L)
      }
    }
  }

  test("Keeps 1000 ms between two notes") {
    TestControl.executeEmbed {
      val stream = PlaybackService.compiledTrackToStream[IO](TrackCompiler.compile(twoNotesTrack, testEnv))
      gaps(stream)
        .map { measured =>
          assertEquals(measured.map(_.toMillis), List(1000L))
        }
    }
  }

  test("Keeps 1000 ms between three notes") {
    TestControl.executeEmbed {
      val stream = PlaybackService.compiledTrackToStream[IO](TrackCompiler.compile(threeNotesTrack, testEnv))
      gaps(stream)
        .map { measured =>
          assertEquals(measured.map(_.toMillis), List(1000L, 1000L))
        }
    }
  }

}
