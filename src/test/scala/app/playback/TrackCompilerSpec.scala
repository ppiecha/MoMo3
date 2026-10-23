package app.playback

import app.domain._
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import munit.FunSuite

class TrackCompilerSpec extends FunSuite {

  given Channel               = Channel.Channel0
  private given TimingContext = valid(TimingContext.from(960, 120))

  test("compile returns InvalidMidiValue when note is out of range") {
    val track = Track.track(Track.time(4), Track.note(200), duration = Some(Track.duration(4)))
    TrackCompiler.compile(track) match {
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
      duration = Some(Track.duration(4, 4)),
      velocity = Some(Track.velocity(100, 90))
    )
    TrackCompiler.compile(track) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        assertEquals(events.size, 5)
        assert(events.count(_.command.isInstanceOf[MidiCommand.ProgramChange]) == 1)
        assert(events.count(_.command.isInstanceOf[MidiCommand.NoteOn]) == 2)
        assert(events.count(_.command.isInstanceOf[MidiCommand.NoteOff]) == 2)
    }
  }

  test("compile skips note-on and note-off when note, velocity, or duration is zero") {
    val tracks = List(
      Track.track(
        Track.time(4),
        Track.note(0),
        duration = Some(Track.duration(4)),
        velocity = Some(Track.velocity(100))
      ),
      Track.track(
        Track.time(4),
        Track.note(60),
        duration = Some(Track.duration(4)),
        velocity = Some(Track.velocity(0))
      ),
      Track.track(
        Track.time(4),
        Track.note(60),
        duration = Some(Track.duration(0)),
        velocity = Some(Track.velocity(100))
      )
    )

    tracks.foreach { track =>
      TrackCompiler.compile(track) match {
        case Invalid(errors) =>
          fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
        case Valid(events) =>
          assert(!events.exists(_.command.isInstanceOf[MidiCommand.NoteOn]))
          assert(!events.exists(_.command.isInstanceOf[MidiCommand.NoteOff]))
      }
    }
  }

  test("compile emits one program change per track with selected instrument") {
    val instrument = Instrument(Instrument.Bank.Melodic, Instrument.Piano.BrightAcousticPiano)
    val track = Track.track(
      Track.time(4, 4),
      Track.note(36, 38),
      duration = Some(Track.duration(4, 4)),
      velocity = Some(Track.velocity(100, 90)),
      instrument = instrument
    )
    val bank = valid(Bank.from(instrument.bank))
    val prog = valid(Program.from(instrument.program))

    TrackCompiler.compile(track) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        val programChanges = events.collect {
          case event if event.command.isInstanceOf[MidiCommand.ProgramChange] => event
        }
        assertEquals(programChanges.size, 1)
        assertEquals(
          programChanges.head,
          AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(Channel.Channel0, bank, prog))
        )
    }
  }

  test("compile emits events for each note in a chord step") {
    val track = Track.track(
      Track.time(4),
      Track.note((60, 64, 67)),
      duration = Some(Track.duration(4)),
      velocity = Some(Track.velocity(100))
    )
    TrackCompiler.compile(track) match {
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
      duration = Some(Track.duration(4)),
      velocity = Some(Track.velocity(100, 90))
    )
    TrackCompiler.compile(track) match {
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
