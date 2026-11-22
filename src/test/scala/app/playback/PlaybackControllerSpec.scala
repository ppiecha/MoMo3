package app.playback

import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.domain.PlaybackPlan.PlaybackPlanData
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite

import scala.concurrent.duration.*

class PlaybackControllerSpec extends CatsEffectSuite {

  test("pause sends all notes off and stop sends all sounds off on every channel") {
    val plan = PlaybackPlanData(
      Seq(
        TimedEvent(
          1.hour,
          AbsoluteMidiEvent(
            Tick.Zero,
            MidiCommand.NoteOff(
              Channel.Channel0,
              valid(Note.from(60)),
              valid(Velocity.from(Velocity.Zero))
            )
          )
        )
      )
    )

    for {
      sent       <- Ref[IO].of(Vector.empty[AbstractMidiEvent])
      controller <- PlaybackController.live(event => sent.update(_ :+ event))
      _          <- controller.play(plan, RepeatPolicy.none)
      _          <- controller.pause
      _          <- controller.stop
      events     <- sent.get
    } yield {
      val expected: Vector[AbstractMidiEvent] =
        Channel.values.toVector.map(channel => AbsoluteMidiEvent(Tick.Zero, MidiCommand.AllNotesOff(channel))) ++
          Channel.values.toVector.map(channel => AbsoluteMidiEvent(Tick.Zero, MidiCommand.AllSoundsOff(channel)))
      assertEquals(events, expected)
    }
  }

  test("replacement playback repeats the full plan after its resumed first pass") {
    val event = AbsoluteMidiEvent(
      Tick.Zero,
      MidiCommand.NoteOff(Channel.Channel0, valid(Note.from(60)), valid(Velocity.from(Velocity.Zero)))
    )
    val fullReplacementPlan = PlaybackPlanData(Seq(TimedEvent(10.millis, event)))
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

  test("plan diff includes changed initial control events but ignores note events and later events") {
    val bank         = valid(Bank.from(0))
    val oldProgram   = valid(Program.from(1))
    val newProgram   = valid(Program.from(2))
    val laterProgram = valid(Program.from(3))
    val note         = valid(Note.from(60))
    val velocity     = valid(Velocity.from(100))
    val laterAt      = valid(Tick.from(4)(using valid(TimingContext.from(960, 120))))
    val oldInitial   = AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(Channel.Channel0, bank, oldProgram))
    val newInitial   = AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(Channel.Channel0, bank, newProgram))
    val oldPlan = PlaybackPlanData(
      Seq(
        TimedEvent(0.millis, oldInitial),
        TimedEvent(0.millis, AbsoluteMidiEvent(Tick.Zero, MidiCommand.NoteOn(Channel.Channel0, note, velocity))),
        TimedEvent(
          1.second,
          AbsoluteMidiEvent(laterAt, MidiCommand.ProgramChange(Channel.Channel0, bank, laterProgram))
        )
      )
    )
    val newPlan = PlaybackPlanData(
      Seq(
        TimedEvent(0.millis, newInitial),
        TimedEvent(0.millis, AbsoluteMidiEvent(Tick.Zero, MidiCommand.NoteOn(Channel.Channel0, note, velocity))),
        TimedEvent(1.second, AbsoluteMidiEvent(laterAt, MidiCommand.ProgramChange(Channel.Channel0, bank, oldProgram)))
      )
    )

    assertEquals(
      PlaybackPlanDiff.changedInitialControlEvents(oldPlan, newPlan).toList,
      List[AbstractMidiEvent](newInitial)
    )
  }

  test("replace sends changed initial control events before resuming the remaining plan") {
    given TimingContext = valid(TimingContext.from(960, 120))
    val bank            = valid(Bank.from(0))
    val oldProgram      = valid(Program.from(1))
    val newProgram      = valid(Program.from(2))
    val note            = valid(Note.from(60))
    val velocity        = valid(Velocity.from(100))
    val middleAt        = valid(Tick.from(4))
    val finalAt         = valid(Tick.from(8))

    def plan(program: Program): PlaybackPlan =
      PlaybackPlanData(
        Seq(
          TimedEvent(
            0.millis,
            AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(Channel.Channel0, bank, program))
          ),
          TimedEvent(
            1.second,
            AbsoluteMidiEvent(
              middleAt,
              MidiCommand.NoteOff(Channel.Channel0, note, valid(Velocity.from(Velocity.Zero)))
            )
          ),
          TimedEvent(
            1.hour,
            AbsoluteMidiEvent(finalAt, MidiCommand.NoteOn(Channel.Channel0, note, velocity))
          )
        )
      )

    val io = for {
      sent       <- Ref[IO].of(Vector.empty[AbstractMidiEvent])
      controller <- PlaybackController.live(event => sent.update(_ :+ event))
      _          <- controller.play(plan(oldProgram), RepeatPolicy.none)
      _          <- IO.sleep(1.second)
      _          <- controller.replace(plan(newProgram), RepeatPolicy.none)
      events     <- sent.get
      _          <- controller.stop
    } yield events.collect { case event @ AbsoluteMidiEvent(_, MidiCommand.ProgramChange(_, _, _)) =>
      event
    }

    val programChanges = TestControl.executeEmbed(io).unsafeRunSync()
    assertEquals(
      programChanges.map(_.command),
      Vector(
        MidiCommand.ProgramChange(Channel.Channel0, bank, oldProgram),
        MidiCommand.ProgramChange(Channel.Channel0, bank, newProgram)
      )
    )
  }

  test("changing tempo preserves musical position between events and while paused") {
    val oldTiming = valid(TimingContext.from(480, 60))
    val newTiming = valid(TimingContext.from(480, 120))
    val note      = valid(Note.from(60))
    val event = AbsoluteMidiEvent(
      valid(Tick.from(2)(using oldTiming)),
      MidiCommand.NoteOff(Channel.Channel0, note, valid(Velocity.from(0)))
    )
    val oldPlan = PlaybackPlanData(Seq(TimedEvent(2.seconds, event)), Some(oldTiming))
    val newPlan = PlaybackPlanData(Seq(TimedEvent(1.second, event)), Some(newTiming))

    val io = for {
      sent           <- Ref[IO].of(Vector.empty[AbstractMidiEvent])
      controller     <- PlaybackController.live(event => sent.update(_ :+ event))
      _              <- controller.play(oldPlan, RepeatPolicy.none)
      _              <- IO.sleep(500.millis)
      _              <- controller.replace(newPlan, RepeatPolicy.none)
      position       <- controller.elapsedTime
      _              <- IO.sleep(250.millis)
      _              <- controller.pause
      _              <- controller.replace(oldPlan, RepeatPolicy.none)
      pausedStatus   <- controller.status
      pausedPosition <- controller.elapsedTime
      _              <- IO.sleep(1.second)
      beforeResume   <- sent.get
      _              <- controller.resume
      _              <- IO.sleep(999.millis)
      beforeEvent    <- sent.get
      _              <- IO.sleep(2.millis)
      afterEvent     <- sent.get
      _              <- controller.stop
    } yield (position, pausedStatus, pausedPosition, beforeResume, beforeEvent, afterEvent)

    val (position, pausedStatus, pausedPosition, beforeResume, beforeEvent, afterEvent) =
      TestControl.executeEmbed(io).unsafeRunSync()
    assertEquals(position, 250.millis)
    assertEquals(pausedStatus, PlaybackStatus.Paused)
    assertEquals(pausedPosition, 1.second)
    assert(!beforeResume.contains(event))
    assert(!beforeEvent.contains(event))
    assert(afterEvent.contains(event))
  }

  test("tempo changes preserve position within the current repeat") {
    val oldTiming = valid(TimingContext.from(480, 60))
    val newTiming = valid(TimingContext.from(480, 120))
    val event = AbsoluteMidiEvent(
      valid(Tick.from(2)(using oldTiming)),
      MidiCommand.NoteOff(Channel.Channel0, valid(Note.from(60)), valid(Velocity.from(0)))
    )
    val oldPlan = PlaybackPlanData(Seq(TimedEvent(2.seconds, event)), Some(oldTiming))
    val newPlan = PlaybackPlanData(Seq(TimedEvent(1.second, event)), Some(newTiming))
    val io = for {
      controller <- PlaybackController.live(_ => IO.unit)
      _          <- controller.play(oldPlan, RepeatPolicy.forever)
      _          <- IO.sleep(2500.millis)
      _          <- controller.replace(newPlan, RepeatPolicy.forever)
      position   <- controller.elapsedTime
      _          <- controller.stop
    } yield position

    assertEquals(TestControl.executeEmbed(io).unsafeRunSync(), 250.millis)
  }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case cats.data.Validated.Valid(value) => value
    case cats.data.Validated.Invalid(errors) =>
      fail(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
