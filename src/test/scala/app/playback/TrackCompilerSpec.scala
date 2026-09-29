package app.playback

import app.domain.*
import app.domain.given
import cats.data.Validated.{Invalid, Valid}
import munit.FunSuite

class TrackCompilerSpec extends FunSuite {

  given Channel = Channel.Ch0

  test("compile returns InvalidMidiValue when note is out of range") {
    val track  = Track.track(Track.time(4), Track.duration(4), Track.note(200))
    val timing = valid(TimingContext.from(960, 120))

    TrackCompiler.compile(track, timing) match {
      case Valid(_) =>
        fail("Expected validation failure for invalid MIDI note")
      case Invalid(errors) =>
        assert(errors.toChain.toList.contains(ValidationError.InvalidMidiValue(200)))
    }
  }

  test("compile emits note-on and note-off for each source note") {
    val track = Track.track(
      Track.time(4, 4),
      Track.duration(4, 4),
      Track.note(60, 62),
      Track.velocity(100, 90)
    )
    val timing = valid(TimingContext.from(960, 120))

    TrackCompiler.compile(track, timing) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        assertEquals(events.size, 4)
        assert(events.head.command.isInstanceOf[MidiCommand.NoteOn])
        assert(events(1).command.isInstanceOf[MidiCommand.NoteOff])
        assert(events(2).command.isInstanceOf[MidiCommand.NoteOn])
        assert(events(3).command.isInstanceOf[MidiCommand.NoteOff])
    }
  }

  private def valid[A](validated: cats.data.ValidatedNec[ValidationError, A]): A = validated match {
    case Valid(value) => value
    case Invalid(errors) =>
      throw new IllegalStateException(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
