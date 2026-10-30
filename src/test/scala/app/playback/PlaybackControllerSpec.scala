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
    val bank = valid(Bank.from(0))
    val oldProgram = valid(Program.from(1))
    val newProgram = valid(Program.from(2))
    val laterProgram = valid(Program.from(3))
    val note = valid(Note.from(60))
    val velocity = valid(Velocity.from(100))
    val laterAt = valid(Tick.from(4)(using valid(TimingContext.from(960, 120))))
    val oldInitial = AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(Channel.Channel0, bank, oldProgram))
    val newInitial = AbsoluteMidiEvent(Tick.Zero, MidiCommand.ProgramChange(Channel.Channel0, bank, newProgram))
    val oldPlan = PlaybackPlanData(
      Seq(
        TimedEvent(0.millis, oldInitial),
        TimedEvent(0.millis, AbsoluteMidiEvent(Tick.Zero, MidiCommand.NoteOn(Channel.Channel0, note, velocity))),
        TimedEvent(1.second, AbsoluteMidiEvent(laterAt, MidiCommand.ProgramChange(Channel.Channel0, bank, laterProgram)))
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
    val bank = valid(Bank.from(0))
    val oldProgram = valid(Program.from(1))
    val newProgram = valid(Program.from(2))
    val note = valid(Note.from(60))
    val velocity = valid(Velocity.from(100))
    val middleAt = valid(Tick.from(4))
    val finalAt = valid(Tick.from(8))

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
      sent <- Ref[IO].of(Vector.empty[AbstractMidiEvent])
      controller <- PlaybackController.live(event => sent.update(_ :+ event))
      _ <- controller.play(plan(oldProgram), RepeatPolicy.none)
      _ <- awaitElapsed(controller)
      _ <- controller.replace(plan(newProgram), RepeatPolicy.none)
      events <- sent.get
      _ <- controller.stop
    } yield events.collect {
      case event @ AbsoluteMidiEvent(_, MidiCommand.ProgramChange(_, _, _)) => event
    }

    val programChanges = TestControl.executeEmbed(io).unsafeRunSync()
    assertEquals(programChanges.map(_.command), Vector(
      MidiCommand.ProgramChange(Channel.Channel0, bank, oldProgram),
      MidiCommand.ProgramChange(Channel.Channel0, bank, newProgram)
    ))
  }

  private def awaitElapsed(controller: PlaybackController): IO[Unit] =
    controller.elapsedTime.flatMap { elapsed =>
      if elapsed > Duration.Zero then IO.unit
      else IO.sleep(1.millis) *> awaitElapsed(controller)
    }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case cats.data.Validated.Valid(value) => value
    case cats.data.Validated.Invalid(errors) =>
      fail(s"Invalid test value: ${errors.toChain.toList.mkString(", ")}")
  }
}
