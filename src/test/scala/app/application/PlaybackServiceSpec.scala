package app.application

import app.config.TimingContext
import app.domain.{given, *}
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.data.Validated
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.concurrent.duration.*

class PlaybackServiceSpec extends ScalaCheckSuite {

  test("compiledTrackToAbsoluteEvents keeps successful events") {
    forAll(PlaybackServiceSpec.genAbsoluteMidiEvents) { events =>
      val compiled = CompiledTrack(events.map(Right(_)))

      assertEquals(PlaybackService.compiledTrackToAbsoluteEvents(compiled), Right(events))
    }
  }

  test("compiledTrackToAbsoluteEvents returns the first failure") {
    val failure = DomainError.PlaybackFailed("boom")
    val first = AbsoluteMidiEvent(
      Tick.zero,
      MidiCommand.NoteOff(
        PlaybackServiceSpec.valid(Channel.from(0)),
        PlaybackServiceSpec.valid(MidiValue[NoteTag](60))
      )
    )
    val second = AbsoluteMidiEvent(
      Tick.zero,
      MidiCommand.NoteOff(
        PlaybackServiceSpec.valid(Channel.from(1)),
        PlaybackServiceSpec.valid(MidiValue[NoteTag](61))
      )
    )

    val compiled = CompiledTrack(Vector(Right(first), Left(failure), Right(second)))

    assertEquals(PlaybackService.compiledTrackToAbsoluteEvents(compiled), Left(failure))
  }

  property("toTimedEvents preserves cardinality") {
    forAll(PlaybackServiceSpec.genAbsoluteMidiEvents, PlaybackServiceSpec.genTimingContext) { (events, timingContext) =>
      val timed = PlaybackService.toTimedEvents(events, timingContext)

      assertEquals(timed.size, events.size)
    }
  }

  property("toTimedEvents sorts events by their absolute time") {
    forAll(PlaybackServiceSpec.genAbsoluteMidiEvents, PlaybackServiceSpec.genTimingContext) { (events, timingContext) =>
      val timed = PlaybackService.toTimedEvents(events, timingContext)
      val times = timed.map(_.event.at.value)

      assertEquals(times, times.sorted)
      assertEquals(timed.map(_.event), events.sortBy(_.at.value))
    }
  }

  property("toTimedEvents computes delay deltas from consecutive timestamps") {
    forAll(PlaybackServiceSpec.genAbsoluteMidiEvents, PlaybackServiceSpec.genTimingContext) { (events, timingContext) =>
      val sorted   = events.sortBy(_.at.value)
      val timed    = PlaybackService.toTimedEvents(events, timingContext)
      val expected = PlaybackServiceSpec.expectedDelays(sorted, timingContext)

      assertEquals(timed.map(_.delay), expected)
    }
  }

  property("toTimedEvents returns an empty sequence for empty input") {
    forAll(PlaybackServiceSpec.genTimingContext) { timingContext =>
      assertEquals(PlaybackService.toTimedEvents(Vector.empty, timingContext), Vector.empty)
    }
  }

  property("toPlaybackPlan flattens tracks and times the combined events") {
    forAll(PlaybackServiceSpec.genCompiledTracks, PlaybackServiceSpec.genTimingContext) { (tracks, timingContext) =>
      val plan      = PlaybackService.toPlaybackPlan(tracks, timingContext)
      val flattened = tracks.flatMap(_.events.collect { case Right(event) => event })
      val expected  = PlaybackService.PlaybackPlan(PlaybackService.toTimedEvents(flattened, timingContext))

      assertEquals(plan, Right(expected))
    }
  }

  test("executePlaybackPlan sends timed events in order") {
    val first = AbsoluteMidiEvent(
      Tick.zero,
      MidiCommand.NoteOn(
        PlaybackServiceSpec.valid(Channel.from(0)),
        PlaybackServiceSpec.valid(MidiValue[NoteTag](60)),
        PlaybackServiceSpec.valid(MidiValue[VelocityTag](100))
      )
    )
    val second = AbsoluteMidiEvent(
      PlaybackServiceSpec.valid(Tick.fromInt(480)),
      MidiCommand.NoteOff(PlaybackServiceSpec.valid(Channel.from(0)), PlaybackServiceSpec.valid(MidiValue[NoteTag](60)))
    )
    val plan = PlaybackService.PlaybackPlan(
      Vector(
        PlaybackService.TimedEvent(0.seconds, first),
        PlaybackService.TimedEvent(10.millis, second)
      )
    )

    val io = for {
      sent   <- Ref[IO].of(Vector.empty[AbsoluteMidiEvent])
      result <- PlaybackService.executePlaybackPlan[IO](plan, event => sent.update(_ :+ event))
      events <- sent.get
    } yield (result, events)

    val (result, events) = TestControl.executeEmbed(io).unsafeRunSync()

    assertEquals(result, Right(()))
    assertEquals(events, Vector(first, second))
  }
}

object PlaybackServiceSpec {

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
    Gen.chooseNum(0, 15).map(value => valid(Channel.from(value)))

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

  val genCompiledTracks: Gen[Vector[CompiledTrack]] =
    Gen.listOf(genAbsoluteMidiEvents.map(events => CompiledTrack(events.map(Right(_))))).map(_.toVector)

  def expectedDelays(events: Seq[AbsoluteMidiEvent], timingContext: TimingContext): Vector[FiniteDuration] =
    events
      .foldLeft((Tick.zero, Vector.empty[FiniteDuration])) { case ((prev, acc), event) =>
        val delta = event.at - prev
        (event.at, acc :+ delta.toMillis(timingContext.ppq, timingContext.bpm))
      }
      ._2
}
