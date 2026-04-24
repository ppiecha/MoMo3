package app.domain

import munit.FunSuite
import TestTracks.*
import app.midi.*
import app.application.TrackCompiler
import scala.concurrent.duration.*

class TrackEventsSpec extends FunSuite {

  test("One note track produces one event") {
    val events = TrackCompiler.compile(oneNoteTrack, testEnv).events.toList
    assertEquals(events.size, 1)
    val event = events.head
    event match {
      case Left(error) => fail(s"Expected event to be a NoteMessage, but got error: $error")
      case Right(ev)   => {
        assertEquals(ev.message.note.get.note, MidiValue.unsafe(60))
        assertEquals(ev.message.note.get.duration, Time(500.millis, Tick.unsafe(480)))
      }
    }
  }

  test("Two notes track produces two events with correct timing") {
    val events = TrackCompiler.compile(twoNotesTrack, testEnv).events.toList
    assertEquals(events.size, 2)
    val List(event1, event2) = events
    event1 match {
      case Left(error) => fail(s"Expected first event to be a NoteMessage, but got error: $error")
      case Right(ev)   => {
        assertEquals(ev.channel, Channel.unsafe(0))
        assertEquals(ev.time, Time(1.second, Tick.unsafe(0)))
        assertEquals(ev.message.note.get.note, MidiValue.unsafe(60))
        assertEquals(ev.message.note.get.duration, Time(4.second, Tick.unsafe(3840)))
      }
    }
    event2 match {
      case Left(error) => fail(s"Expected second event to be a NoteMessage, but got error: $error")
      case Right(ev)   => {
        assertEquals(ev.channel, Channel.unsafe(0))
        assertEquals(ev.time, Time(1.second, Tick.unsafe(960)))
        assertEquals(ev.message.note.get.note, MidiValue.unsafe(62))
        assertEquals(ev.message.note.get.duration, Time(4.second, Tick.unsafe(3840)))
      }
    }
  }

  test("Three notes track produces three events with correct timing") {
    val events = TrackCompiler.compile(threeNotesTrack, testEnv).events.toList
    assertEquals(events.size, 3)
    val List(event1, event2, event3) = events
    event1 match {
      case Left(error) => fail(s"Expected first event to be a NoteMessage, but got error: $error")
      case Right(ev)   => {
        assertEquals(ev.channel, Channel.unsafe(0))
        assertEquals(ev.time, Time(1.second, Tick.unsafe(0)))
        assertEquals(ev.message.note.get.note, MidiValue.unsafe(60))
        assertEquals(ev.message.note.get.duration, Time(4.second, Tick.unsafe(3840)))
      }
    } 
      event2 match {
      case Left(error) => fail(s"Expected second event to be a NoteMessage, but got error: $error")
      case Right(ev)   => {
        assertEquals(ev.channel, Channel.unsafe(0))
        assertEquals(ev.time, Time(1.second, Tick.unsafe(960)))
        assertEquals(ev.message.note.get  .note, MidiValue.unsafe(64))
        assertEquals(ev.message.note.get.duration, Time(3.second, Tick.unsafe(2880))) 
      }
    }
    event3 match {
      case Left(error) => fail(s"Expected third event to be a NoteMessage, but got error: $error")
      case Right(ev)   => {
        assertEquals(ev.channel, Channel.unsafe(0))
        assertEquals(ev.time, Time(2.seconds, Tick.unsafe(1920)))
        assertEquals(ev.message.note.get.note, MidiValue.unsafe(67))
        assertEquals(ev.message.note.get.duration, Time(2.seconds, Tick.unsafe(1920)))
      }
    }
  }
}