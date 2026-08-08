package app.domain

import munit.CatsEffectSuite
import TestTracks.*
import app.config.Environment
import app.config.stdInput
import app.application.*
import app.domain.*
import app.shared.IsValid
import cats.data.Validated
import cats.syntax.all.*

class TrackSpec extends CatsEffectSuite {

  def validOrFail[A](validated: IsValid[A]): A = validated match {
    case Validated.Valid(value) => value
    case Validated.Invalid(errors) => fail(s"Expected valid value but got errors: $errors")
  }

  test("One note track midi stream should produce NoteOn and NoteOff message") {

    val (ppq, bpm, channel, note, velocity) = validOrFail(
      (
        Ppq.from(960),
        Bpm.from(60),
        Channel.from(0),
        MidiValue.from(60),
        Velocity.from(100)
      ).mapN((p, b, c, n, v) => (p, b, c, n, v))
    )

    val env = validOrFail(Environment.from(ppq = ppq.value, bpm = bpm.value))
    val events = TrackCompiler.compile(oneNoteTrack, env.timingContext).events

    val expectedEvents = List(
      AbsoluteMidiEvent(Tick.zero, MidiCommand.NoteOn(channel, note, velocity)),
      AbsoluteMidiEvent(validOrFail(Tick.fromInt(480)), MidiCommand.NoteOff(channel, note))
    )

    events.toList.sequence match
      case Left(error) => fail(s"Expected valid events but got errors: $error")
      case Right(events) => assertEquals(events, expectedEvents)

  }
}
