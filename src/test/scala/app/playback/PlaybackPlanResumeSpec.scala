package app.playback

import app.domain.*
import app.domain.given
import cats.data.Validated.{Invalid, Valid}
import munit.FunSuite

import scala.concurrent.duration.*

class PlaybackPlanResumeSpec extends FunSuite {

  test("resumeFrom keeps full plan when elapsed is non-positive") {
    val plan = samplePlan

    assertEquals(PlaybackPlanResume.resumeFrom(plan, 0.millis), plan)
    assertEquals(PlaybackPlanResume.resumeFrom(plan, -5.millis), plan)
  }

  test("resumeFrom drops fully elapsed head event") {
    val resumed = PlaybackPlanResume.resumeFrom(samplePlan, 10.millis)

    assertEquals(resumed.events.map(_.delay), Vector(20.millis, 30.millis))
  }

  test("resumeFrom returns empty plan when elapsed exceeds total duration") {
    val resumed = PlaybackPlanResume.resumeFrom(samplePlan, 100.millis)

    assertEquals(resumed, PlaybackPlan.empty)
  }

  private val samplePlan: PlaybackPlan = PlaybackPlan(
    Vector(
      TimedEvent(10.millis, AbsoluteMidiEvent(Tick.zero, MidiCommand.NoteOn(Channel.Ch0, note(60), velocity(100)))),
      TimedEvent(20.millis, AbsoluteMidiEvent(tick(480), MidiCommand.NoteOff(Channel.Ch0, note(60)))),
      TimedEvent(30.millis, AbsoluteMidiEvent(tick(960), MidiCommand.NoteOn(Channel.Ch0, note(62), velocity(100))))
    )
  )

  private def note(value: Int): Note         = valid(MidiValue[NoteTag](value))
  private def velocity(value: Int): Velocity = valid(MidiValue[VelocityTag](value))
  private def tick(value: Int): Tick         = valid(Tick.fromInt(value))

  private def valid[A](validated: cats.data.ValidatedNec[ValidationError, A]): A = validated match {
    case Valid(value) => value
    case Invalid(errors) =>
      throw new IllegalStateException(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
