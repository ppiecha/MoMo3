package app.playback

import app.domain._
import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite

import scala.concurrent.duration._

class PlaybackControllerSpec extends CatsEffectSuite {

  test("pause sends all notes off and stop sends all sounds off on every channel") {
    val plan = PlaybackPlan(
      Vector(
        TimedEvent(1.hour, AbsoluteMidiEvent(Tick.Zero, MidiCommand.NoteOff(Channel.Channel0, valid(Note.from(60)))))
      )
    )

    for {
      sent       <- Ref[IO].of(Vector.empty[AbsoluteMidiEvent])
      controller <- PlaybackController.live(event => sent.update(_ :+ event))
      _          <- controller.play(plan)
      _          <- controller.pause
      _          <- controller.stop
      events     <- sent.get
    } yield {
      val expected =
        Channel.values.toVector.map(MidiCommand.AllNotesOff.apply) ++
          Channel.values.toVector.map(MidiCommand.AllSoundsOff.apply)
      assertEquals(events.map(_.command), expected)
    }
  }

  test("replacement playback repeats the full plan after its resumed first pass") {
    val event = AbsoluteMidiEvent(
      Tick.Zero,
      MidiCommand.NoteOff(Channel.Channel0, valid(Note.from(60)))
    )
    val fullReplacementPlan = PlaybackPlan(Vector(TimedEvent(10.millis, event)))
    val resumedPlan         = PlaybackPlanResume.resumeFrom(fullReplacementPlan, 10.millis)

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

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case cats.data.Validated.Valid(value) => value
    case cats.data.Validated.Invalid(errors) =>
      throw new IllegalStateException(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
