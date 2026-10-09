package app.playback

import app.domain._
import app.playback.TrackFileParser.classNameFromFilePath
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.nio.file.WatchEvent
import scala.concurrent.duration._

class TrackDirectoryMonitorSpec extends FunSuite {

  test("watch events trigger scans only for Scala files or overflow") {
    assert(!TrackDirectoryMonitor.requiresScan(List(watchEvent("notes.txt"))))
    assert(TrackDirectoryMonitor.requiresScan(List(watchEvent("track.scala"))))
    assert(TrackDirectoryMonitor.requiresScan(List(overflowEvent)))
  }

  test("monitor builds plan from Music.scala entrypoint") {
    val directory = Files.createTempDirectory("track-monitor-music-entrypoint")
    val musicFile = Files.createTempFile("track-monitor-music-entrypoint-file", ".scala")

    writeTrack(directory.resolve("Piano.scala"), 64)
    writeTrack(directory.resolve("Drums.scala"), 36)
    writeMusicCollectionFromTrackObjects(musicFile, List("Piano", "Drums"))

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor
      .live(
        directory = directory,
        playback = harness,
        timing = timing,
        musicFile = musicFile,
        policy = RepeatPolicy.none,
        pollInterval = 10.millis
      )
      .unsafeRunSync()

    val result = monitor.scanOnce.unsafeRunSync()

    assertEquals(extractFirstNoteFromPlan(result), 64)
    assertEquals(harness.replaceCalls.size, 1)
    assert(result.size >= 4)
  }

  test("scanOnce updates playback only when Music.scala compiles") {
    val directory = Files.createTempDirectory("track-monitor-music-compile-gate")
    val musicFile = Files.createTempFile("track-monitor-music-compile-gate-file", ".scala")

    writeTrack(directory.resolve("Piano.scala"), 60)
    writeMusicCollectionFromTrackObjects(musicFile, List("Piano"))

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor
      .live(
        directory = directory,
        playback = harness,
        timing = timing,
        musicFile = musicFile,
        policy = RepeatPolicy.none,
        pollInterval = 10.millis
      )
      .unsafeRunSync()

    val firstResult = monitor.scanOnce.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(firstResult), 60)
    assertEquals(harness.replaceCalls.size, 1)

    Files.writeString(
      musicFile,
      """object Music extends app.syntax.MusicFile {
        |  def play: Seq[app.domain.Track] = Seq(MissingTrack())
        |}
        |""".stripMargin
    )

    val secondResult = monitor.scanOnce.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(secondResult), 60)
    assertEquals(harness.replaceCalls.size, 1)
  }

  test("missing music file keeps current plan unchanged") {
    val directory = Files.createTempDirectory("track-monitor-music-missing")
    val musicFile = directory.resolve("Music.scala")

    writeTrack(directory.resolve("Piano.scala"), 52)
    Files.writeString(
      musicFile,
      """object Music extends app.syntax.MusicFile {
        |  def play: Seq[app.domain.Track] = Seq(Piano())
        |}
        |""".stripMargin
    )

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor
      .live(
        directory = directory,
        playback = harness,
        timing = timing,
        musicFile = musicFile,
        policy = RepeatPolicy.none,
        pollInterval = 10.millis
      )
      .unsafeRunSync()

    val firstResult = monitor.scanOnce.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(firstResult), 52)
    assertEquals(harness.replaceCalls.size, 1)

    Files.delete(musicFile)

    val secondResult = monitor.scanOnce.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(secondResult), 52)
    assertEquals(harness.replaceCalls.size, 1)
  }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case cats.data.Validated.Valid(value) => value
    case cats.data.Validated.Invalid(_)   => throw new IllegalStateException("invalid test value")
  }

  private def writeTrack(path: Path, note: Int): Unit =
    Files.writeString(
      path,
      s"""object ${classNameFromFilePath(path)} {
         |  given app.domain.Channel = app.domain.Channel.Ch0
         |  def apply(): app.domain.Track = app.domain.Track.track(
         |    timeGen = app.domain.Track.time(1),
         |    durGen = app.domain.Track.duration(1),
         |    noteGen = app.domain.Track.note($note)
         |  )
         |}
         |""".stripMargin
    )

  private def writeMusicCollectionFromTrackObjects(path: Path, objects: List[String]): Unit = {
    val tracks = objects.map(name => s"$name()").mkString(",\n      ")

    Files.writeString(
      path,
      s"""object Music extends app.syntax.MusicFile {
         |  def play: Seq[app.domain.Track] = Seq(
         |      $tracks
         |  )
         |}
         |""".stripMargin
    )
  }

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

  private def extractFirstNoteFromPlan(plan: PlaybackPlan): Int =
    plan.events
      .collectFirst { case TimedEvent(_, AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, note, _))) =>
        note.value
      }
      .getOrElse(throw new IllegalStateException("missing NoteOn event in playback plan"))

  private final class TrackDirectoryMonitorTestHarness extends PlaybackController {
    var replaceCalls: List[PlaybackPlan] = Nil

    override def play(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] = IO.unit
    override def pause: IO[Unit]                                          = IO.unit
    override def resume: IO[Unit]                                         = IO.unit
    override def stop: IO[Unit]                                           = IO.unit
    override def replace(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] = {
      replaceCalls = replaceCalls :+ plan
      IO.unit
    }
    override def elapsedTime: IO[FiniteDuration] = IO.pure(0.millis)
  }
}
