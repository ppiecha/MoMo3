package app.playback

import app.domain._
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import munit.FunSuite

class TrackCompilerSpec extends FunSuite {

  given Channel = Channel.Ch0

  test("compile returns InvalidMidiValue when note is out of range") {
    val track  = Track.track(Track.time(4), Track.note(200), durGen = Some(Track.duration(4)))
    val timing = valid(TimingContext.from(960, 120))

    TrackCompiler.compile(track, timing) match {
      case Valid(_) =>
        fail("Expected validation failure for invalid MIDI note")
      case Invalid(errors) =>
        assert(errors.toChain.toList.contains(DomainError.InvalidMidiValue(200)))
    }
  }

  test("compile emits note-on and note-off for each source note") {
    val track = Track.track(
      Track.time(4, 4),
      Track.note(60, 62),
      durGen = Some(Track.duration(4, 4)),
      velGen = Some(Track.velocity(100, 90))
    )
    val timing = valid(TimingContext.from(960, 120))

    TrackCompiler.compile(track, timing) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        assertEquals(events.size, 6)
        assert(events.count(_.command.isInstanceOf[MidiCommand.ProgramChange]) == 2)
        assert(events.count(_.command.isInstanceOf[MidiCommand.NoteOn]) == 2)
        assert(events.count(_.command.isInstanceOf[MidiCommand.NoteOff]) == 2)
    }
  }

  test("compile emits events for each note in a chord step") {
    val track = Track.track(
      Track.time(4),
      Track.note((60, 64, 67)),
      durGen = Some(Track.duration(4)),
      velGen = Some(Track.velocity(100))
    )
    val timing = valid(TimingContext.from(960, 120))

    TrackCompiler.compile(track, timing) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        assertEquals(events.size, 7)
        assert(
          events.forall(event =>
            event.command.isInstanceOf[MidiCommand.ProgramChange] ||
              event.command.isInstanceOf[MidiCommand.NoteOn] ||
              event.command.isInstanceOf[MidiCommand.NoteOff]
          )
        )
    }
  }

  test("compile fails hard when generator lengths are inconsistent") {
    val track = Track.track(
      Track.time(4, 4),
      Track.note(60, 62),
      durGen = Some(Track.duration(4)),
      velGen = Some(Track.velocity(100, 90))
    )
    val timing = valid(TimingContext.from(960, 120))

    TrackCompiler.compile(track, timing) match {
      case Valid(_) =>
        fail("Expected validation failure for mismatched generator lengths")
      case Invalid(errors) =>
        assert(
          errors.toChain.toList.contains(
            DomainError.TrackLengthMismatch(time = 2, duration = 1, notes = 2, velocity = 2)
          )
        )
    }
  }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case Valid(value) => value
    case Invalid(errors) =>
      throw new IllegalStateException(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
