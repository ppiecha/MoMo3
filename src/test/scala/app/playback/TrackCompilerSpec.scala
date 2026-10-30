package app.playback

import app.domain._
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
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
      velocity = Track.velocity(100)
    )
    TrackCompiler.compile(track) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        val commands = events.toList.collect { case AbsoluteMidiEvent(_, command) => command }
        assertEquals(events.length, 5)
        assertEquals(commands.count {
          case MidiCommand.ProgramChange(_, _, _) => true
          case _                                  => false
        }, 1)
        assertEquals(commands.count {
          case MidiCommand.NoteOn(_, _, _) => true
          case _                           => false
        }, 2)
        assertEquals(commands.count {
          case MidiCommand.NoteOff(_, _, _) => true
          case _                            => false
        }, 2)
        assertEquals(
          commands.collect { case MidiCommand.NoteOn(_, _, velocity) => velocity.value },
          List(100, 100)
        )
    }
  }

  test("compile skips note-on and note-off when note, velocity, or duration is zero") {
    val tracks = List(
      Track.track(
        Track.time(4),
        Track.note(0),
        duration = Some(Track.duration(4)),
        velocity = Track.velocity(100)
      ),
      Track.track(
        Track.time(4),
        Track.note(60),
        duration = Some(Track.duration(4)),
        velocity = Track.velocity(0)
      ),
      Track.track(
        Track.time(4),
        Track.note(60),
        duration = Some(Track.duration(0)),
        velocity = Track.velocity(100)
      )
    )

    tracks.foreach { track =>
      TrackCompiler.compile(track) match {
        case Invalid(errors) =>
          fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
        case Valid(events) =>
          assert(!events.exists {
            case AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, _, _))  => true
            case _                                                  => false
          })
          assert(!events.exists {
            case AbsoluteMidiEvent(_, MidiCommand.NoteOff(_, _, _)) => true
            case _                                                  => false
          })
      }
    }
  }

  test("compile emits one program change per track with selected instrument") {
    val instrument = Instrument(Bank.Default, Instrument.Piano.BrightAcousticPiano)
    val track = Track.track(
      Track.time(4, 4),
      Track.note(36, 38),
      duration = Some(Track.duration(4, 4)),
      velocity = Track.velocity(100, 90),
      instrument = Some(instrument)
    )
    val bank = valid(Bank.from(instrument.bank))
    val prog = valid(Program.from(instrument.program))

    TrackCompiler.compile(track) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        val programChanges = events.toList.collect {
          case event @ AbsoluteMidiEvent(_, MidiCommand.ProgramChange(_, _, _)) => event
        }
        assertEquals(programChanges.size, 1)
        assertEquals(programChanges.headOption, Some(AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(Channel.Channel0, bank, prog))))
    }
  }

  test("compile emits events for each note in a chord step") {
    val track = Track.track(
      Track.time(4),
      Track.note((60, 64, 67)),
      duration = Some(Track.duration(4)),
      velocity = Track.velocity(100)
    )
    TrackCompiler.compile(track) match {
      case Invalid(errors) =>
        fail(s"Expected valid events but got errors: ${errors.toChain.toList.mkString(", ")}")
      case Valid(events) =>
        assertEquals(events.length, 7)
        assert(
          events.toList.forall {
            case AbsoluteMidiEvent(_, MidiCommand.ProgramChange(_, _, _)) => true
            case AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, _, _))        => true
            case AbsoluteMidiEvent(_, MidiCommand.NoteOff(_, _, _))       => true
            case _                                                        => false
          }
        )
    }
  }

  test("compile fails hard when generator lengths are inconsistent") {
    val track = Track.track(
      Track.time(4, 4),
      Track.note(60, 62),
      duration = Some(Track.duration(4)),
      velocity = Track.velocity(100, 90)
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
      fail(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
