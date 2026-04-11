package app.model

import munit.CatsEffectSuite
import cats.effect.*
import TestTracks.*
import app.midi.*
import scala.concurrent.duration.*

class TrackEventsSpec extends CatsEffectSuite {
  
  test("One note track produces one event") {

    oneNoteTrack.eventList[IO].value
      .map {
        case Right(events)     => events
        case Left(domainError) => throw new RuntimeException(domainError.toString)
      }
      .map { events =>
        assertEquals(events.size, 1)
        val event = events.head
        assertEquals(event.channel, Channel.unsafe(0))
        assertEquals(event.time.duration, 1.second)
        event.message match {
          case Message.NoteMessage(note, time, velocity) =>
            assertEquals(note, MidiValue.unsafe(60))
            assertEquals(time.duration, 500.millis)
          case _ => fail("Expected a NoteMessage")
        }
        
      }
  }

  test("Two notes track produces two events with correct timing") {
    
    twoNotesTrack.eventList[IO].value
      .map {
        case Right(events)     => events
        case Left(domainError) => throw new RuntimeException(domainError.toString)
      }
      .map { events =>
        assertEquals(events.size, 2)
        val List(event1, event2) = events.toList
        assertEquals(event1.channel, Channel.unsafe(0))
        assertEquals(event1.time, Time(1.second, Tick.unsafe(0)))
        event1.message match {
          case Message.NoteMessage(note, duration, velocity) =>
            assertEquals(note, MidiValue.unsafe(60))
            assertEquals(duration, Time(1.second, Tick.unsafe(960)))
          case _ => fail("Expected a NoteMessage")
        }
        assertEquals(event2.channel, Channel.unsafe(0))
        assertEquals(event2.time, Time(2.seconds, Tick.unsafe(960)))
        event2.message match {
          case Message.NoteMessage(note, duration, velocity) =>
            assertEquals(note, MidiValue.unsafe(62))
            assertEquals(duration, Time(1.second, Tick.unsafe(960)))
          case _ => fail("Expected a NoteMessage")
        }
      }
  }

  test("Three notes track produces three events with correct timing") {
    
    threeNotesTrack.eventList[IO].value
      .map {
        case Right(events)     => events
        case Left(domainError) => throw new RuntimeException(domainError.toString)
      }
      .map { events =>
        assertEquals(events.size, 3)
        val List(event1, event2, event3) = events.toList
        assertEquals(event1.channel, Channel.unsafe(0))
        assertEquals(event1.time, Time(1.second, Tick.unsafe(0)))
        event1.message match {
          case Message.NoteMessage(note, duration, velocity) =>
            assertEquals(note, MidiValue.unsafe(60))
            assertEquals(duration, Time(1.second, Tick.unsafe(960)))
          case _ => fail("Expected a NoteMessage")
        }
        assertEquals(event2.channel, Channel.unsafe(0))
        assertEquals(event2.time, Time(1.second, Tick.unsafe(960)))
        event2.message match {
          case Message.NoteMessage(note, duration, velocity) =>
            assertEquals(note, MidiValue.unsafe(64))
            assertEquals(duration, Time(1.second, Tick.unsafe(960)))
          case _ => fail("Expected a NoteMessage")
        }
        assertEquals(event3.channel, Channel.unsafe(0))
        assertEquals(event3.time, Time(2.seconds, Tick.unsafe(1920)))
        event3.message match {
          case Message.NoteMessage(note, duration, velocity) =>
            assertEquals(note, MidiValue.unsafe(67))
            assertEquals(duration, Time(2.seconds, Tick.unsafe(1920)))
          case _ => fail("Expected a NoteMessage")
        }
      }
  }
}
