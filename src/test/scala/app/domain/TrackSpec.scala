package app.domain

import munit.CatsEffectSuite
import TestTracks.*
import app.config.Environment
import app.domain.*
import cats.data.{Validated, ValidatedNec}
import cats.syntax.all.*
import app.playback.TrackCompiler

class TrackSpec extends CatsEffectSuite {

  given Channel = Channel.Ch0

  def validOrFail[A](validated: Either[DomainError, A]): A = validated match {
    case Right(value) => value
    case Left(errors) => fail(s"Expected valid value but got errors: $errors")
  }

  def validOrFail[A](validated: ValidatedNec[ValidationError, A]): A = validated match {
    case Validated.Valid(value)    => value
    case Validated.Invalid(errors) => fail(s"Expected valid value but got errors: $errors")
  }

  test("time should create time generators from one or more values") {
    assertEquals(Track.time(2), TimeGen(Seq(2.0)))
    assertEquals(Track.time(1, 2, 3), TimeGen(Seq(1.0, 2.0, 3.0)))
  }

  test("duration should create duration generators from one or more values") {
    assertEquals(Track.duration(2), DurationGen(Seq(2.0)))
    assertEquals(Track.duration(1, 2, 3), DurationGen(Seq(1.0, 2.0, 3.0)))
  }

  test("note should create note generators from one or more values") {
    assertEquals(Track.note(60), Generator.NoteGen(Seq(60)))
    assertEquals(Track.note(60, 62, 64), Generator.NoteGen(Seq(60, 62, 64)))
  }

  test("velocity should create velocity generators from one or more values") {
    assertEquals(Track.velocity(70), Generator.VelocityGen(Seq(70)))
    assertEquals(Track.velocity(70, 80, 90), Generator.VelocityGen(Seq(70, 80, 90)))
  }

  test("track overloads should provide default duration and velocity generators") {
    val expected = Track(
      timeGen = Track.time(1, 2),
      durGen = Track.duration(1, 2),
      noteGen = Track.note(60, 62),
      velGen = Track.velocity(Velocity.Default, Velocity.Default)
    )

    assertEquals(Track.track(Track.time(1, 2), Track.note(60, 62)), expected)
    assertEquals(
      Track.track(Track.time(1, 2), Track.duration(3, 4), Track.note(60, 62)),
      expected.copy(durGen = Track.duration(3, 4))
    )
  }

  test("rest should create a silent track with matching time and duration") {
    val rest = Track.rest(4)

    assertEquals(rest.timeGen, Track.time(4))
    assertEquals(rest.durGen, Track.duration(4))
    assertEquals(rest.noteGen, Track.note(Note.Zero))
    assertEquals(rest.velGen, Track.velocity(Velocity.Zero))
  }

  test("track concatenation should concatenate every generator") {
    val first  = Track.track(Track.time(1), Track.duration(2), Track.note(60), Track.velocity(70))
    val second = Track.track(Track.time(3), Track.duration(4), Track.note(62), Track.velocity(80))

    assertEquals(
      first ++ second,
      Track.track(Track.time(1, 3), Track.duration(2, 4), Track.note(60, 62), Track.velocity(70, 80))
    )
  }

  test("track concatenation should preserve correct duration type") {
    val first  = Track.track(Track.time(1, 2), Track.note(60, 62))
    val second = Track.rest(0.5)

    assertEquals(
      (first ++ second).durGen,
      Track.duration(1, 2, 0.5)
    )
  }

  test("muted should preserve timing and notes while zeroing velocity") {
    val track = Track.track(Track.time(1, 2), Track.duration(3, 4), Track.note(60, 62), Track.velocity(70, 80))

    assertEquals(track.muted, track.copy(velGen = Track.velocity(Velocity.Zero, Velocity.Zero)))
  }

  test("One note track midi stream should produce NoteOn and NoteOff message") {

    val (ppq, bpm, note, velocity) = validOrFail(
      (
        Ppq.from(960),
        Bpm.from(60),
        MidiValue[NoteTag](60),
        MidiValue[VelocityTag](100)
      ).mapN((p, b, n, v) => (p, b, n, v))
    )

    val env     = validOrFail(Environment.from(ppq = ppq.value, bpm = bpm.value))
    val events  = TrackCompiler.compile(oneNoteTrack, env.timingContext).events
    val channel = Channel.Ch0
    val expectedEvents = List(
      AbsoluteMidiEvent(Tick.zero, MidiCommand.NoteOn(channel, note, velocity)),
      AbsoluteMidiEvent(validOrFail(Tick.fromInt(480)), MidiCommand.NoteOff(channel, note))
    )

    events.toList.sequence match
      case Left(error)   => fail(s"Expected valid events but got errors: $error")
      case Right(events) => assertEquals(events, expectedEvents)

  }
}
