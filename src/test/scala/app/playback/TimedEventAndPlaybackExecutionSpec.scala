package app.playback

import app.domain.Channel._
import app.domain._
import app.midi.toMidiMessages
import cats.data.Validated
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import javax.sound.midi.ShortMessage
import scala.concurrent.duration._

class TimedEventAndPlaybackExecutionSpec extends ScalaCheckSuite {

  property("fromAbsoluteEvents preserves cardinality") {
    forAll(
      TimedEventAndPlaybackExecutionSpec.genAbsoluteMidiEvents,
      TimedEventAndPlaybackExecutionSpec.genTimingContext
    ) { (events, timingContext) =>
      val timed = TimedEvent.fromAbsoluteEvents(events)(using timingContext)

      assertEquals(timed.size, events.size)
    }
  }

  property("fromAbsoluteEvents sorts events by their absolute time") {
    forAll(
      TimedEventAndPlaybackExecutionSpec.genAbsoluteMidiEvents,
      TimedEventAndPlaybackExecutionSpec.genTimingContext
    ) { (events, timingContext) =>
      val timed = TimedEvent.fromAbsoluteEvents(events)(using timingContext)
      val times = timed.map(_.event.at.value)

      assertEquals(times, times.sorted)
      assertEquals(timed.map(_.event), events.sortBy(TimedEventAndPlaybackExecutionSpec.sortKey))
    }
  }

  property("fromAbsoluteEvents computes delay deltas from consecutive timestamps") {
    forAll(
      TimedEventAndPlaybackExecutionSpec.genAbsoluteMidiEvents,
      TimedEventAndPlaybackExecutionSpec.genTimingContext
    ) { (events, timingContext) =>
      val sorted   = events.sortBy(TimedEventAndPlaybackExecutionSpec.sortKey)
      val timed    = TimedEvent.fromAbsoluteEvents(events)(using timingContext)
      val expected = TimedEventAndPlaybackExecutionSpec.expectedDelays(sorted, timingContext)

      assertEquals(timed.map(_.delay), expected)
    }
  }

  test("fromAbsoluteEvents sorts equal timestamps by command priority") {
    val timing   = TimedEventAndPlaybackExecutionSpec.valid(TimingContext.from(960, 120))
    val at       = TimedEventAndPlaybackExecutionSpec.valid(Tick.from(8)(using timing))
    val channel  = Channel.Channel0
    val note     = TimedEventAndPlaybackExecutionSpec.valid(Note.from(60))
    val velocity = TimedEventAndPlaybackExecutionSpec.valid(Velocity.from(100))
    val bank     = TimedEventAndPlaybackExecutionSpec.valid(Bank.from(1))
    val program  = TimedEventAndPlaybackExecutionSpec.valid(Program.from(2))
    val control  = TimedEventAndPlaybackExecutionSpec.valid(Control.from(7))
    val controlV = TimedEventAndPlaybackExecutionSpec.valid(Control.from(90))

    val events = Vector(
      AbsoluteMidiEvent(at, MidiCommand.NoteOff(channel, note)),
      AbsoluteMidiEvent(at, MidiCommand.ProgramChange(channel, bank, program)),
      AbsoluteMidiEvent(at, MidiCommand.ControlChange(channel, control, controlV)),
      AbsoluteMidiEvent(at, MidiCommand.NoteOn(channel, note, velocity))
    )

    val ordered = TimedEvent.fromAbsoluteEvents(events)(using timing).map(_.event.command)

    assertEquals(
      ordered,
      Vector(
        MidiCommand.ControlChange(channel, control, controlV),
        MidiCommand.ProgramChange(channel, bank, program),
        MidiCommand.NoteOff(channel, note),
        MidiCommand.NoteOn(channel, note, velocity)
      )
    )
  }

  property("fromAbsoluteEvents returns an empty sequence for empty input") {
    forAll(TimedEventAndPlaybackExecutionSpec.genTimingContext) { timingContext =>
      assertEquals(TimedEvent.fromAbsoluteEvents(Vector.empty)(using timingContext), Vector.empty)
    }
  }

  test("executeWithProgress sends timed events in order") {
    given TimingContext = TimedEventAndPlaybackExecutionSpec.valid(TimingContext.from(960, 120))
    val first = AbsoluteMidiEvent(
      Tick.Zero,
      MidiCommand.NoteOn(
        Channel.Channel0,
        TimedEventAndPlaybackExecutionSpec.valid(Note.from(60)),
        TimedEventAndPlaybackExecutionSpec.valid(Velocity.from(100))
      )
    )
    val second = AbsoluteMidiEvent(
      TimedEventAndPlaybackExecutionSpec.valid(Tick.from(8)),
      MidiCommand.NoteOff(Channel.Channel0, TimedEventAndPlaybackExecutionSpec.valid(Note.from(60)))
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

  test("all notes off and all sounds off encode as channel control changes") {
    val notesOff  = MidiCommand.AllNotesOff(Channel4).toMidiMessages.head
    val soundsOff = MidiCommand.AllSoundsOff(Channel4).toMidiMessages.head

    assertEquals(
      (notesOff.getCommand, notesOff.getChannel, notesOff.getData1, notesOff.getData2),
      (ShortMessage.CONTROL_CHANGE, 4, 123, 0)
    )
    assertEquals(
      (soundsOff.getCommand, soundsOff.getChannel, soundsOff.getData1, soundsOff.getData2),
      (ShortMessage.CONTROL_CHANGE, 4, 120, 0)
    )
  }
}

object TimedEventAndPlaybackExecutionSpec {

  def sortKey(event: AbsoluteMidiEvent): (Int, Int) =
    (event.at.value, commandOrder(event.command))

  private def commandOrder(command: MidiCommand): Int =
    command match {
      case MidiCommand.ControlChange(_, _, _) => 0
      case MidiCommand.AllNotesOff(_)         => 0
      case MidiCommand.AllSoundsOff(_)        => 0
      case MidiCommand.ProgramChange(_, _, _) => 1
      case MidiCommand.NoteOff(_, _, _)       => 2
      case MidiCommand.NoteOn(_, _, _)        => 3
    }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case Validated.Valid(value) => value
    case Validated.Invalid(errors) =>
      throw new IllegalStateException(s"Generated invalid value: $errors")
  }

  private val genTimingContextForTicks = valid(TimingContext.from(960, 120))

  val genTick: Gen[Tick] =
    Gen.chooseNum(0, 20_000).map(value => valid(Tick.from(value.toDouble)(using genTimingContextForTicks)))

  val genTimingContext: Gen[TimingContext] =
    for {
      ppq <- Gen.chooseNum(1, 1_920)
      bpm <- Gen.chooseNum(1, 300)
    } yield valid(TimingContext.from(ppq, bpm))

  val genChannel: Gen[Channel] =
    Gen.oneOf(Channel.values.toSeq)

  val genMidiValue: Gen[Note] =
    Gen.chooseNum(0, 127).map(value => valid(Note.from(value)))

  val genVelocity: Gen[Velocity] =
    Gen.chooseNum(0, 127).map(value => valid(Velocity.from(value)))

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
      .foldLeft((Tick.Zero, Vector.empty[FiniteDuration])) { case ((prev, acc), event) =>
        val delta = event.at - prev
        (event.at, acc :+ delta.toMillis(using timingContext))
      }
      ._2
}
