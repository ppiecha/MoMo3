package app.playback

import app.domain.{_, given}
import cats.data.Validated
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.concurrent.duration._

class PlaybackServiceSpec extends ScalaCheckSuite {

  property("fromAbsoluteEvents preserves cardinality") {
    forAll(PlaybackServiceSpec.genAbsoluteMidiEvents, PlaybackServiceSpec.genTimingContext) { (events, timingContext) =>
      val timed = TimedEvent.fromAbsoluteEvents(events, timingContext)

      assertEquals(timed.size, events.size)
    }
  }

  property("fromAbsoluteEvents sorts events by their absolute time") {
    forAll(PlaybackServiceSpec.genAbsoluteMidiEvents, PlaybackServiceSpec.genTimingContext) { (events, timingContext) =>
      val timed = TimedEvent.fromAbsoluteEvents(events, timingContext)
      val times = timed.map(_.event.at.value)

      assertEquals(times, times.sorted)
      assertEquals(timed.map(_.event), events.sortBy(PlaybackServiceSpec.sortKey))
    }
  }

  property("fromAbsoluteEvents computes delay deltas from consecutive timestamps") {
    forAll(PlaybackServiceSpec.genAbsoluteMidiEvents, PlaybackServiceSpec.genTimingContext) { (events, timingContext) =>
      val sorted   = events.sortBy(PlaybackServiceSpec.sortKey)
      val timed    = TimedEvent.fromAbsoluteEvents(events, timingContext)
      val expected = PlaybackServiceSpec.expectedDelays(sorted, timingContext)

      assertEquals(timed.map(_.delay), expected)
    }
  }

  test("fromAbsoluteEvents sorts equal timestamps by command priority") {
    val at       = PlaybackServiceSpec.valid(Tick.fromInt(480))
    val channel  = Channel.Ch0
    val note     = PlaybackServiceSpec.valid(MidiValue[NoteTag](60))
    val velocity = PlaybackServiceSpec.valid(MidiValue[VelocityTag](100))
    val bank     = PlaybackServiceSpec.valid(MidiValue[BankTag](1))
    val program  = PlaybackServiceSpec.valid(MidiValue[ProgramTag](2))
    val control  = PlaybackServiceSpec.valid(MidiValue[ControlTag](7))
    val controlV = PlaybackServiceSpec.valid(MidiValue[ControlTag](90))
    val timing   = PlaybackServiceSpec.valid(TimingContext.from(960, 120))

    val events = Vector(
      AbsoluteMidiEvent(at, MidiCommand.NoteOff(channel, note)),
      AbsoluteMidiEvent(at, MidiCommand.ProgramChange(channel, bank, program)),
      AbsoluteMidiEvent(at, MidiCommand.ControlChange(channel, control, controlV)),
      AbsoluteMidiEvent(at, MidiCommand.NoteOn(channel, note, velocity))
    )

    val ordered = TimedEvent.fromAbsoluteEvents(events, timing).map(_.event.command)

    assertEquals(
      ordered,
      Vector(
        MidiCommand.ControlChange(channel, control, controlV),
        MidiCommand.ProgramChange(channel, bank, program),
        MidiCommand.NoteOn(channel, note, velocity),
        MidiCommand.NoteOff(channel, note)
      )
    )
  }

  property("fromAbsoluteEvents returns an empty sequence for empty input") {
    forAll(PlaybackServiceSpec.genTimingContext) { timingContext =>
      assertEquals(TimedEvent.fromAbsoluteEvents(Vector.empty, timingContext), Vector.empty)
    }
  }

  test("executeWithProgress sends timed events in order") {
    val first = AbsoluteMidiEvent(
      Tick.zero,
      MidiCommand.NoteOn(
        Channel.Ch0,
        PlaybackServiceSpec.valid(MidiValue[NoteTag](60)),
        PlaybackServiceSpec.valid(MidiValue[VelocityTag](100))
      )
    )
    val second = AbsoluteMidiEvent(
      PlaybackServiceSpec.valid(Tick.fromInt(480)),
      MidiCommand.NoteOff(Channel.Ch0, PlaybackServiceSpec.valid(MidiValue[NoteTag](60)))
    )
    val plan = PlaybackPlan(
      Vector(
        TimedEvent(0.seconds, first),
        TimedEvent(10.millis, second)
      )
    )

    val io = for {
      sent   <- Ref[IO].of(Vector.empty[AbsoluteMidiEvent])
      result <- PlaybackExecution.executeWithProgress[IO](plan, event => sent.update(_ :+ event))
      events <- sent.get
    } yield (result, events)

    val (result, events) = TestControl.executeEmbed(io).unsafeRunSync()

    assertEquals(result, 10.millis)
    assertEquals(events, Vector(first, second))
  }
}

object PlaybackServiceSpec {

  def sortKey(event: AbsoluteMidiEvent): (Int, Int) =
    (event.at.value, commandOrder(event.command))

  private def commandOrder(command: MidiCommand): Int =
    command match {
      case MidiCommand.ControlChange(_, _, _) => 0
      case MidiCommand.ProgramChange(_, _, _) => 1
      case MidiCommand.NoteOn(_, _, _)        => 2
      case MidiCommand.NoteOff(_, _, _)       => 3
    }

  private def valid[A](validated: cats.data.ValidatedNec[ValidationError, A]): A = validated match {
    case Validated.Valid(value) => value
    case Validated.Invalid(errors) =>
      throw new IllegalStateException(s"Generated invalid value: $errors")
  }

  val genTick: Gen[Tick] =
    Gen.chooseNum(0, 20_000).map(value => valid(Tick.fromInt(value)))

  val genTimingContext: Gen[TimingContext] =
    for {
      ppq <- Gen.chooseNum(1, 1_920)
      bpm <- Gen.chooseNum(1, 300)
    } yield valid(TimingContext.from(ppq, bpm))

  val genChannel: Gen[Channel] =
    Gen.oneOf(Channel.values.toSeq)

  val genMidiValue: Gen[Note] =
    Gen.chooseNum(0, 127).map(value => valid(MidiValue[NoteTag](value)))

  val genVelocity: Gen[Velocity] =
    Gen.chooseNum(0, 127).map(value => valid(MidiValue[VelocityTag](value)))

  val genMidiCommand: Gen[MidiCommand] =
    for {
      channel   <- genChannel
      note      <- genMidiValue
      velocity  <- genVelocity
      useNoteOn <- Gen.oneOf(true, false)
    } yield
      if useNoteOn then MidiCommand.NoteOn(channel, note, velocity) else MidiCommand.NoteOff(channel, note, velocity)

  val genAbsoluteMidiEvent: Gen[AbsoluteMidiEvent] =
    for {
      at      <- genTick
      command <- genMidiCommand
    } yield AbsoluteMidiEvent(at, command)

  val genAbsoluteMidiEvents: Gen[Vector[AbsoluteMidiEvent]] =
    Gen.listOf(genAbsoluteMidiEvent).map(_.toVector)

  def expectedDelays(events: Seq[AbsoluteMidiEvent], timingContext: TimingContext): Vector[FiniteDuration] =
    events
      .foldLeft((Tick.zero, Vector.empty[FiniteDuration])) { case ((prev, acc), event) =>
        val delta = event.at - prev
        (event.at, acc :+ delta.toMillis(timingContext.ppq, timingContext.bpm))
      }
      ._2
}
