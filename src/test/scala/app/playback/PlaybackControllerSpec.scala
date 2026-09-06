package app.playback

import app.domain.*
import app.domain.given
import munit.FunSuite

import scala.concurrent.duration.*

class PlaybackControllerSpec extends FunSuite {

  test("replacement playback repeats the full plan after its resumed first pass") {
    val event = AbsoluteMidiEvent(
      Tick.zero,
      MidiCommand.NoteOff(valid(Channel.from(0)), valid(MidiValue[NoteTag](60)))
    )
    val fullReplacementPlan = PlaybackPlan(Vector(TimedEvent(10.millis, event)))
    val resumedPlan = PlaybackPlanResume.resumeFrom(fullReplacementPlan, 10.millis)

    assertEquals(
      PlaybackController.nextRepeat(fullReplacementPlan, RepeatPolicy.fixed(1)),
      Some(fullReplacementPlan -> RepeatPolicy.none)
    )
    assertNotEquals(
      PlaybackController.nextRepeat(fullReplacementPlan, RepeatPolicy.fixed(1)),
      Some(resumedPlan -> RepeatPolicy.none)
    )
    assertEquals(PlaybackController.nextRepeat(fullReplacementPlan, RepeatPolicy.none), None)
  }

  private def valid[A](validated: cats.data.ValidatedNec[ValidationError, A]): A = validated match {
    case cats.data.Validated.Valid(value) => value
    case cats.data.Validated.Invalid(errors) =>
      throw new IllegalStateException(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}