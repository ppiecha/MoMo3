package app.playback

import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.domain.PlaybackPlan.EmptyPlaybackPlan
import app.domain.PlaybackPlan.PlaybackPlanData
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import munit.FunSuite

import scala.concurrent.duration.*

class PlaybackPlanResumeSpec extends FunSuite {

  private given TimingContext = valid(TimingContext.from(960, 120))

  test("resumeFrom keeps full plan when elapsed is non-positive") {
    val plan = samplePlan

    assertEquals(PlaybackPlanResume.resumeFrom(plan, 0.millis), plan)
    assertEquals(PlaybackPlanResume.resumeFrom(plan, -5.millis), plan)
  }

  test("resumeFrom drops fully elapsed head event") {
    val resumed = PlaybackPlanResume.resumeFrom(samplePlan, 10.millis)

    assertEquals(planEvents(resumed).map(_.delay), Vector(20.millis, 30.millis))
  }

  test("resumeFrom shortens the first event when elapsed is within its delay") {
    val resumed = PlaybackPlanResume.resumeFrom(samplePlan, 5.millis)

    assertEquals(planEvents(resumed).map(_.delay), Vector(5.millis, 20.millis, 30.millis))
  }

  test("resumeFrom returns empty plan when elapsed exceeds total duration") {
    val resumed = PlaybackPlanResume.resumeFrom(samplePlan, 100.millis)

    assertEquals(resumed, EmptyPlaybackPlan)
  }

  private val samplePlan: PlaybackPlan = PlaybackPlanData(
    Seq(
      TimedEvent(
        10.millis,
        AbsoluteMidiEvent(Tick.Zero, MidiCommand.NoteOn(Channel.Channel0, note(60), velocity(100)))
      ),
      TimedEvent(
        20.millis,
        AbsoluteMidiEvent(tick(8), MidiCommand.NoteOff(Channel.Channel0, note(60), velocity(0)))
      ),
      TimedEvent(30.millis, AbsoluteMidiEvent(tick(4), MidiCommand.NoteOn(Channel.Channel0, note(62), velocity(100))))
    )
  )

  private def planEvents(plan: PlaybackPlan): Vector[TimedEvent] = plan match {
    case EmptyPlaybackPlan            => Vector.empty
    case PlaybackPlanData(events)     => events.toVector
  }

  private def note(value: Int): Note           = valid(Note.from(value))
  private def velocity(value: Int): Velocity   = valid(Velocity.from(value))
  private def tick(noteDuration: Double): Tick = valid(Tick.from(noteDuration))

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case Valid(value) => value
    case Invalid(errors) =>
      fail(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
