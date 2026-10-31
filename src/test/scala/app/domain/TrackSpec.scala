package app.domain

import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.playback.TrackCompiler
import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all.*
import munit.CatsEffectSuite

import TestTracks.*

class TrackSpec extends CatsEffectSuite {

  given Channel               = Channel.Channel0
  private given TimingContext = validOrFail(TimingContext.from(960, 60))

  def validOrFail[A](validated: Either[DomainError, A]): A = validated match {
    case Right(value) => value
    case Left(errors) => fail(s"Expected valid value but got errors: $errors")
  }

  def validOrFail[A](validated: ValidatedNec[DomainError, A]): A = validated match {
    case Validated.Valid(value)    => value
    case Validated.Invalid(errors) => fail(s"Expected valid value but got errors: $errors")
  }

  test("time should create time generators from one or more values") {
    assertEquals(Track.time(2), TimeGen(NonEmptyList.one(2.0)))
    assertEquals(Track.time(1, 2, 3), TimeGen(NonEmptyList.of(1.0, 2.0, 3.0)))
  }

  test("duration should create duration generators from one or more values") {
    assertEquals(Track.duration(2), DurationGen(NonEmptyList.one(2.0)))
    assertEquals(Track.duration(1, 2, 3), DurationGen(NonEmptyList.of(1.0, 2.0, 3.0)))
  }

  test("note should create note generators from one or more values") {
    assertEquals(Track.note(60), Some(Generator.NoteGen(NonEmptyList.one(Chord.Voiced(NonEmptyList.one(60))))))
    assertEquals(
      Track.note(60, 62, 64),
      Some(
        Generator.NoteGen(
          NonEmptyList.of(
            Chord.Voiced(NonEmptyList.one(60)),
            Chord.Voiced(NonEmptyList.one(62)),
            Chord.Voiced(NonEmptyList.one(64))
          )
        )
      )
    )
  }

  test("note constants should cover chromatic pitches from A0 through C8") {
    val pitches = List(
      Note.a0,
      Note.as0,
      Note.b0,
      Note.c1,
      Note.cs1,
      Note.d1,
      Note.ds1,
      Note.e1,
      Note.f1,
      Note.fs1,
      Note.g1,
      Note.gs1,
      Note.a1,
      Note.as1,
      Note.b1,
      Note.c2,
      Note.cs2,
      Note.d2,
      Note.ds2,
      Note.e2,
      Note.f2,
      Note.fs2,
      Note.g2,
      Note.gs2,
      Note.a2,
      Note.as2,
      Note.b2,
      Note.c3,
      Note.cs3,
      Note.d3,
      Note.ds3,
      Note.e3,
      Note.f3,
      Note.fs3,
      Note.g3,
      Note.gs3,
      Note.a3,
      Note.as3,
      Note.b3,
      Note.c4,
      Note.cs4,
      Note.d4,
      Note.ds4,
      Note.e4,
      Note.f4,
      Note.fs4,
      Note.g4,
      Note.gs4,
      Note.a4,
      Note.as4,
      Note.b4,
      Note.c5,
      Note.cs5,
      Note.d5,
      Note.ds5,
      Note.e5,
      Note.f5,
      Note.fs5,
      Note.g5,
      Note.gs5,
      Note.a5,
      Note.as5,
      Note.b5,
      Note.c6,
      Note.cs6,
      Note.d6,
      Note.ds6,
      Note.e6,
      Note.f6,
      Note.fs6,
      Note.g6,
      Note.gs6,
      Note.a6,
      Note.as6,
      Note.b6,
      Note.c7,
      Note.cs7,
      Note.d7,
      Note.ds7,
      Note.e7,
      Note.f7,
      Note.fs7,
      Note.g7,
      Note.gs7,
      Note.a7,
      Note.as7,
      Note.b7,
      Note.c8
    )

    assertEquals(pitches, (21 to 108).toList)
    assertEquals(
      Track.note(Note.a0, Note.as0, Note.b0, Note.c1, Note.cs1, Note.c8),
      Track.note(21, 22, 23, 24, 25, 108)
    )
    assertEquals(
      List(Note.c, Note.cs, Note.d, Note.ds, Note.e, Note.f, Note.fs, Note.g, Note.gs, Note.a, Note.as, Note.b),
      (60 to 71).toList
    )
  }

  test("note should allow tuple chords mixed with single notes") {
    assertEquals(
      Track.note((60, 64, 67), 30, 40),
      Some(
        Generator.NoteGen(
          NonEmptyList.of(
            Chord.Voiced(NonEmptyList(60, List(64, 67))),
            Chord.Voiced(NonEmptyList.one(30)),
            Chord.Voiced(NonEmptyList.one(40))
          )
        )
      )
    )
  }

  test("transpose should keep rest notes (0) untouched") {
    import app.syntax.Extensions.*
    assertEquals(
      (Track.note(0) ++ Track.note(60)).transpose(24),
      Track.note(0, 84)
    )
  }

  test("PlaybackPlan.fromTracks should exclude inactive tracks") {
    val active   = Track.track(Track.time(4), Track.note(60))
    val inactive = Track.track(Track.time(4), Track.note(62), isActive = false)
    val tracks   = Tracks.from(Validated.validNec(List(active, inactive))).right.getOrElse(fail("invalid tracks"))

    val noteOns = (PlaybackPlan.fromTracks(tracks).right.getOrElse(fail("invalid plan")) match
      case PlaybackPlan.PlaybackPlanData(events) => events.map(_.toString).mkString
      case PlaybackPlan.EmptyPlaybackPlan        => ""
    )
    assert(noteOns.contains("60"))
    assert(!noteOns.contains("62"))
  }

  test("velocity should create velocity generators from one or more values") {
    assertEquals(Track.velocity(70), Some(Generator.VelocityGen(NonEmptyList.one(70))))
    assertEquals(Track.velocity(70, 80, 90), Some(Generator.VelocityGen(NonEmptyList.of(70, 80, 90))))
  }

  test("track factory leaves optional generators unset unless provided") {
    val expected = Track(
      timing = Track.time(1, 2),
      notes = Track.note(60, 62),
      duration = None,
      velocity = None,
      instrument = None,
      source = Some("TrackSpec.scala"),
      isActive = true
    )

    assertEquals(Track.track(Track.time(1, 2), Track.note(60, 62)), expected)
    assertEquals(
      Track.track(Track.time(1, 2), Track.note(60, 62), duration = Some(Track.duration(3, 4))),
      expected.copy(duration = Some(Track.duration(3, 4)))
    )
  }

  test("track factory should keep provided instrument") {
    val instrument = Instrument(Bank.Percussion, Instrument.PercussionKit.ElectronicKit)

    val track = Track.track(
      Track.time(1, 2),
      Track.note(60, 62),
      instrument = Some(instrument)
    )

    assertEquals(track.instrument, Some(instrument))
  }

  test("rest should create a track with no note, duration, or velocity generators") {
    val rest = Track.rest(4)

    assertEquals(rest.timing, Track.time(4))
    assertEquals(rest.duration, None)
    assertEquals(rest.notes, None)
    assertEquals(rest.velocity, None)
  }

  test("track concatenation should concatenate every generator") {
    val first = Track.track(
      Track.time(1),
      Track.note(60),
      duration = Some(Track.duration(2)),
      velocity = Track.velocity(70)
    )
    val second = Track.track(
      Track.time(3),
      Track.note(62),
      duration = Some(Track.duration(4)),
      velocity = Track.velocity(80)
    )

    assertEquals(
      first ++ second,
      Track.track(
        Track.time(1, 3),
        Track.note(60, 62),
        duration = Some(Track.duration(2, 4)),
        velocity = Track.velocity(70, 80)
      )
    )
  }

  test("track concatenation should preserve correct duration type") {
    val first =
      Track.track(Track.time(1), Track.note(60), duration = Some(Track.duration(1)))
    val second = Track.rest(0.5)

    assertEquals(
      (first ++ second).duration,
      Some(Track.duration(1))
    )
  }

  test("muted should preserve timing and notes while zeroing velocity") {
    val track = Track.track(
      Track.time(1, 2),
      Track.note(60, 62),
      duration = Some(Track.duration(3, 4)),
      velocity = Track.velocity(70, 80)
    )

    assertEquals(
      track.muted,
      track.copy(velocity = Some(Generator.velocity(Velocity.Zero, Velocity.Zero)))
    )
  }

  test("One note track midi stream should produce NoteOn and NoteOff message") {

    val (note, velocity) = validOrFail(
      (
        Note.from(60),
        Velocity.from(Velocity.Default)
      ).mapN((n, v) => (n, v))
    )

    val events  = TrackCompiler.compile(oneNoteTrack)
    val channel = Channel.Channel0
    val bank    = validOrFail(Bank.from(Instrument.Default.bank))
    val program = validOrFail(Program.from(Instrument.Default.program))
    val expectedEvents: List[AbstractMidiEvent] = List(
      AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(channel, bank, program)),
      AbsoluteMidiEvent(Tick.Zero, MidiCommand.NoteOn(channel, note, velocity)),
      AbsoluteMidiEvent(
        validOrFail(Tick.from(8)),
        MidiCommand.NoteOff(channel, note, validOrFail(Velocity.from(Velocity.Zero)))
      )
    )

    events match
      case Validated.Invalid(errors) => fail(s"Expected valid events but got errors: $errors")
      case Validated.Valid(events)   => assertEquals(events.toList, expectedEvents)
  }
}
