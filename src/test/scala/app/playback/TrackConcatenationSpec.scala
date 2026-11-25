package app.playback

import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.syntax.Extensions.*
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import munit.FunSuite

import scala.language.implicitConversions

class TrackConcatenationSpec extends FunSuite {
  given Channel            = Channel.Channel0
  given Option[Instrument] = None
  private given TimingContext = TimingContext.from(960, 120) match {
    case Valid(context)  => context
    case Invalid(errors) => fail(s"Invalid timing context: $errors")
  }

  test("repeated pattern with a rest keeps durations and velocities aligned") {
    val pattern = Track.track(
      timing = Track.time(8, 8, 16, 16, 8).repeat(2),
      notes = Track.note(0, 51, 0, 48, 0).repeat(2).transpose(36),
      velocity = Track.velocity(55)
    ) ++ Track.rest(1)
    val track = pattern.repeat(4)

    assertEquals(track.timing.length, 44)
    assertEquals(track.notes.map(_.length), Some(44))
    assertEquals(track.duration.map(_.length), Some(44))
    assertEquals(track.velocity.map(_.length), Some(44))
    assertEquals(track.duration.map(_.values.toList), Some(track.timing.values.toList))
    val expectedVelocities = List.fill(4)(List.fill(10)(55) :+ 0).flatten
    assertEquals(track.velocity, Some(Generator.VelocityGen(cats.data.NonEmptyList.fromListUnsafe(expectedVelocities))))

    val events = TrackCompiler.compile(track) match {
      case Valid(result)   => result
      case Invalid(errors) => fail(s"Expected valid events but got $errors")
    }
    val velocities = events.toList.collect { case AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, _, velocity)) =>
      velocity.value
    }
    assertEquals(velocities, List.fill(16)(55))
  }

  test("concatenation fills missing generators on either side") {
    val defaults = Track.track(Track.time(1, 2), None)
    val explicit = Track.track(
      Track.time(3, 4),
      Track.note(60, 62),
      duration = Track.duration(5, 6),
      velocity = Track.velocity(80)
    )

    val before = defaults ++ explicit
    val after  = explicit ++ defaults
    assertEquals(before.notes, Track.note(0, 0, 60, 62))
    assertEquals(after.notes, Track.note(60, 62, 0, 0))
    assertEquals(before.duration, Track.duration(1, 2, 5, 6))
    assertEquals(after.duration, Track.duration(5, 6, 1, 2))
    assertEquals(before.velocity, Track.velocity(Velocity.Default, Velocity.Default, 80, 80))
    assertEquals(after.velocity, Track.velocity(80, 80, Velocity.Default, Velocity.Default))
    assert(TrackCompiler.compile(before).isValid)
    assert(TrackCompiler.compile(after).isValid)
  }

  test("concatenation leaves optional generators absent when both sides use defaults") {
    val track = Track.track(Track.time(1), None) ++ Track.track(Track.time(2), None)
    assertEquals(track.notes, None)
    assertEquals(track.duration, None)
    assertEquals(track.velocity, None)
    assert(TrackCompiler.compile(track).isValid)
  }

  test("short velocity patterns fall back to their own first value, not the other track's") {
    val first  = Track.track(Track.time(1, 1, 1), Track.note(60, 61, 62), velocity = Track.velocity(70, 80))
    val second = Track.track(Track.time(1, 1, 1), Track.note(63, 64, 65), velocity = Track.velocity(90, 100))
    val track  = first ++ second
    assertEquals(track.velocity, Track.velocity(70, 80, 70, 90, 100, 90))
    val events = TrackCompiler.compile(track) match {
      case Valid(result)   => result
      case Invalid(errors) => fail(s"Expected valid events but got $errors")
    }
    assertEquals(
      events.toList.collect { case AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, _, velocity)) =>
        velocity.value
      },
      List(70, 80, 70, 90, 100, 90)
    )
  }

  test("surplus velocities do not shift the next track and are still validated") {
    val second     = Track.track(Track.time(1), Track.note(62), velocity = Track.velocity(90))
    val validFirst = Track.track(Track.time(1), Track.note(60), velocity = Track.velocity(70, 80))
    val validTrack = validFirst ++ second
    val events = TrackCompiler.compile(validTrack) match {
      case Valid(result)   => result
      case Invalid(errors) => fail(s"Expected valid events but got $errors")
    }
    assertEquals(
      events.toList.collect { case AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, _, velocity)) =>
        velocity.value
      },
      List(70, 90)
    )

    val first = Track.track(Track.time(1), Track.note(60), velocity = Track.velocity(70, 200))
    val track = first ++ second
    assertEquals(track.velocity, Track.velocity(70, 90, 200))
    assert(TrackCompiler.compile(track).isInvalid)
  }

  test("explicitly mismatched duration lengths still fail validation") {
    val invalid = Track.track(Track.time(1, 1), Track.note(60, 62), duration = Track.duration(1))
    val track   = invalid ++ Track.rest(1)
    assertEquals(track.duration.map(_.length), Some(2))
    assert(TrackCompiler.compile(track).isInvalid)
  }
}
