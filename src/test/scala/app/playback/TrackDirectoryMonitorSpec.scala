package app.playback

import app.domain.*
import app.domain.given
import app.playback.TrackFileCompiler.classNameFromFilePath
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import munit.FunSuite

import java.nio.file.{Files, Path, StandardWatchEventKinds, WatchEvent}
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
    assertEquals(RepeatPolicy.fixed(3).remaining, 3)
    assertEquals(RepeatPolicy.forever.remaining, Int.MaxValue)
  }

  test("watch events trigger scans only for Scala files or overflow") {
    assert(!TrackDirectoryMonitor.requiresScan(List(watchEvent("notes.txt"))))
    assert(TrackDirectoryMonitor.requiresScan(List(watchEvent("track.scala"))))
    assert(TrackDirectoryMonitor.requiresScan(List(overflowEvent)))
  }

  test("directory monitor discovers scala tracks and compiles them") {
    val directory = Files.createTempDirectory("track-monitor")
    writeMusic(directory.resolve("track.scala"), 60)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileCompiler.compileAndEvaluateFile,
      compiler = track => TrackCompiler.compile(track, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce.unsafeRunSync()
    assertEquals(result.size, 1)
    assertEquals(harness.playCalls.size, 1)
  }

  test("thesis 1: without another scan after save, playback still uses old parsed track") {
    val directory = Files.createTempDirectory("track-monitor-thesis1")
    val trackFile = directory.resolve("Track1.scala")
    writeMusic(trackFile, 38)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileCompiler.compileAndEvaluateFile,
      compiler = track => TrackCompiler.compile(track, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    monitor.scanOnce.unsafeRunSync()
    assertEquals(extractSingleNote(harness.playCalls.last.head), 38)

    writeMusic(trackFile, 40)
    assertEquals(extractSingleNote(harness.playCalls.last.head), 38)
  }

  test("thesis 2: after save and rescan, parser reads updated file contents") {
    val directory = Files.createTempDirectory("track-monitor-thesis2")
    val trackFile = directory.resolve("Track1.scala")
    writeMusic(trackFile, 38)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileCompiler.compileAndEvaluateFile,
      compiler = track => TrackCompiler.compile(track, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    monitor.scanOnce.unsafeRunSync()
    writeMusic(trackFile, 40)
    monitor.scanOnce.unsafeRunSync()

    assertEquals(extractSingleNote(harness.playCalls.last.head), 40)
  }

  test("thesis 3: TrackFileCompiler returns updated note after file change") {
    val directory = Files.createTempDirectory("track-file-compiler-thesis3")
    val scalaFile = directory.resolve("Track1.scala")

    writeMusic(scalaFile, 38)
    val first = TrackFileCompiler.compileAndEvaluateFile(scalaFile)
    writeMusic(scalaFile, 40)
    val second = TrackFileCompiler.compileAndEvaluateFile(scalaFile)

    assertEquals(extractSingleNoteFromCompiledTrack(validString(first)), 38)
    assertEquals(extractSingleNoteFromCompiledTrack(validString(second)), 40)
  }

  private def valid[A](validated: cats.data.ValidatedNec[ValidationError, A]): A = validated match {
    case Valid(value) => value
    case Invalid(_)   => throw new IllegalStateException("invalid test value")
  }

  private def writeMusic(path: Path, note: Int): Unit =
    Files.writeString(
      path,
      s"""import app.domain.*
         |import app.domain.Generator.*
         |
         |object ${classNameFromFilePath(path)} {
         |  def play(): Track = Track(
         |    channel = Channel.from(0),
         |    timeGen = TimeGen(Seq(1)),
         |    durGen = DurationGen(Seq(1)),
         |    noteGen = NoteGen(Seq($note))
         |  )
         |}
         |""".stripMargin
    )

  private def watchEvent(fileName: String): WatchEvent[Path] =
    new WatchEvent[Path] {
      override def kind: WatchEvent.Kind[Path] = StandardWatchEventKinds.ENTRY_MODIFY
      override def count: Int                  = 1
      override def context: Path               = Path.of(fileName)
    }

  private val overflowEvent: WatchEvent[AnyRef] =
    new WatchEvent[AnyRef] {
      override def kind: WatchEvent.Kind[AnyRef] = StandardWatchEventKinds.OVERFLOW
      override def count: Int                    = 1
      override def context: AnyRef               = null
    }

  private def extractSingleNote(track: Track): Int =
    track.noteGen match {
      case Generator.NoteGen(notes) =>
        notes.headOption.getOrElse(throw new IllegalStateException("missing note in test track"))
      case _ =>
        throw new IllegalStateException("unexpected generator kind in test track")
    }

  private def extractSingleNoteFromCompiledTrack(track: Track): Int =
    extractSingleNote(track)

  private def validString[A](validated: cats.data.ValidatedNec[String, A]): A = validated match {
    case Valid(value) => value
    case Invalid(_)   => throw new IllegalStateException("invalid test value")
  }

  private final class TrackDirectoryMonitorTestHarness extends PlaybackController {
    var playCalls: List[List[Track]] = Nil

    override def play(tracks: List[Track], timing: TimingContext, policy: RepeatPolicy): IO[Unit] = {
      playCalls = playCalls :+ tracks
      IO.unit
    }

    override def pause: IO[Unit]  = IO.unit
    override def resume: IO[Unit] = IO.unit
    override def stop: IO[Unit]   = IO.unit
    override def replace(tracks: List[Track], timing: TimingContext, policy: RepeatPolicy): IO[Unit] = {
      playCalls = playCalls :+ tracks
      IO.unit
    }

    override def elapsedTime: IO[FiniteDuration] = IO.pure(0.millis)
  }
}
