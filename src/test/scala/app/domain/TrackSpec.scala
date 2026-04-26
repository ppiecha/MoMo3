package app.domain

import munit.CatsEffectSuite
import TestTracks.*
import app.config.Environment
import app.midi.ReactiveSynth
import app.config.stdInput
import cats.effect.IO
import app.application.*
import app.midi.*
import app.domain.*
import cats.syntax.all.*

class TrackSpec extends CatsEffectSuite {

  test("One note track midi stream should produce NoteOn and NoteOff message") {
    val env = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
    val events = TrackCompiler.compile(oneNoteTrack, env).events

    val expectedEvents = List(
      AbsoluteMidiEvent(Tick.unsafe(0), MidiCommand.NoteOn(Channel.unsafe(0), MidiValue.unsafe(60), MidiValue.unsafe(100))),
      AbsoluteMidiEvent(Tick.unsafe(480), MidiCommand.NoteOff(Channel.unsafe(0), MidiValue.unsafe(60)))
    )

    events.toList.sequence match
      case Left(error) => fail(s"Expected valid events but got errors: ${error}")
      case Right(events) => assertEquals(events, expectedEvents)
    

  }
}