package app.playback

import app.domain.*
import app.domain.given
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import munit.FunSuite

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*

class TrackDirectoryMonitorSpec extends FunSuite {

  test("resumeFrom trims the plan from the current elapsed moment") {
    val plan = PlaybackPlan(
      Vector(
        TimedEvent(
          10.millis,
          AbsoluteMidiEvent(
            Tick.zero,
            MidiCommand
              .NoteOn(valid(Channel.from(0)), valid(MidiValue[NoteTag](60)), valid(MidiValue[VelocityTag](100)))
          )
        ),
        TimedEvent(
          20.millis,
          AbsoluteMidiEvent(
            valid(Tick.fromInt(480)),
            MidiCommand.NoteOff(valid(Channel.from(0)), valid(MidiValue[NoteTag](60)))
          )
        ),
        TimedEvent(
          5.millis,
          AbsoluteMidiEvent(
            valid(Tick.fromInt(960)),
            MidiCommand.NoteOn(
              valid(Channel.from(0)),
              valid(MidiValue[NoteTag](62)),
              valid(MidiValue[VelocityTag](100))
            )
          )
        )
      )
    )

    val resumed = PlaybackPlanResume.resumeFrom(plan, 25.millis)

    assertEquals(resumed.events.map(_.delay), Vector(5.millis, 5.millis))
  }

  test("repeat policy supports fixed and infinite loops") {
    assertEquals(PlaybackRepeatPolicy.fixed(3).remaining, 3)
    assertEquals(PlaybackRepeatPolicy.forever.remaining, Int.MaxValue)
  }

  test("directory monitor discovers scala tracks and compiles them") {
    val directory = Files.createTempDirectory("track-monitor")
    Files.writeString(directory.resolve("track-a.scala"), "channel=0\ntime=1,2\nduration=1\nnote=60\nvelocity=100\n")

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()
    val parser: Path => ValidatedNec[String, Track] = _ =>
      Valid(
        Track(
          valid(Channel.from(0)).validNec,
          Generator.TimeGen(Seq(1.0, 2.0)),
          Generator.DurationGen(Seq(1.0)),
          Generator.NoteGen(Seq(60)),
          Some(Generator.VelocityGen(Seq(100)))
        )
      )

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = parser,
      compiler = track => TrackCompiler.compile(track, timing),
      playback = harness,
      timing = timing,
      policy = PlaybackRepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce.unsafeRunSync()
    assertEquals(result.size, 1)
    assertEquals(harness.playCalls.size, 1)
  }

  private def valid[A](validated: cats.data.ValidatedNec[ValidationError, A]): A = validated match {
    case Valid(value) => value
    case Invalid(_)   => throw new IllegalStateException("invalid test value")
  }

  private final class TrackDirectoryMonitorTestHarness extends PlaybackController {
    var playCalls: List[List[Track]] = Nil

    override def play(tracks: List[Track], timing: TimingContext, policy: PlaybackRepeatPolicy): IO[Unit] = {
      playCalls = playCalls :+ tracks
      IO.unit
    }

    override def pause: IO[Unit]  = IO.unit
    override def resume: IO[Unit] = IO.unit
    override def stop: IO[Unit]   = IO.unit
    override def replace(tracks: List[Track], timing: TimingContext, policy: PlaybackRepeatPolicy): IO[Unit] = {
      playCalls = playCalls :+ tracks
      IO.unit
    }

    override def elapsedTime: IO[FiniteDuration] = IO.pure(0.millis)
  }
}
