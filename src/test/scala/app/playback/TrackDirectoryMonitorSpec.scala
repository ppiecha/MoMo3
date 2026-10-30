package app.playback

import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.playback.PlaybackStatus
import app.playback.TrackFileParser.classNameFromFilePath
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.nio.file.WatchEvent
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

class TrackDirectoryMonitorSpec extends FunSuite {

  test("watch events trigger scans only for Scala files or overflow") {
    assert(!TrackDirectoryMonitor.requiresScan(List(watchEvent("notes.txt"))))
    assert(TrackDirectoryMonitor.requiresScan(List(watchEvent("track.scala"))))
    assert(TrackDirectoryMonitor.requiresScan(List(overflowEvent)))
  }

  test("monitor builds plan from Music.scala entrypoint") {
    val directory = Files.createTempDirectory("track-monitor-music-entrypoint")
    val musicFile = directory.resolve("Music.scala")

    writeTrack(directory.resolve("Bass0.scala"), 64)
    writeTrack(directory.resolve("Drums9.scala"), 36)
    writeMusicCollectionFromTrackObjects(musicFile, List("Bass0", "Drums9"))

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

    val result = monitor.scanAndPlay.unsafeRunSync()

    assertEquals(extractFirstNoteFromPlan(result), 64)
    assertEquals(harness.replaceCalls.size, 0)
    assert(planEvents(result).size >= 4)
  }

  test("scanOnce updates playback only when Music.scala compiles") {
    val directory = Files.createTempDirectory("track-monitor-music-compile-gate")
    val musicFile = directory.resolve("Music.scala")

    writeTrack(directory.resolve("Bass0.scala"), 60)
    writeMusicCollectionFromTrackObjects(musicFile, List("Bass0"))

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

    val firstResult = monitor.scanReplaceAndResume.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(firstResult), 60)
    assertEquals(harness.replaceCalls.size, 1)

    Files.writeString(
      musicFile,
      """object Music extends app.syntax.MusicFile {
        |  def play: Seq[app.domain.Track] = Seq(MissingTrack())
        |}
        |""".stripMargin
    )

    val secondResult = monitor.scanReplaceAndResume.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(secondResult), 60)
    assertEquals(harness.replaceCalls.size, 1)
  }

  test("watcher rescans a changed track and replaces playback") {
    val directory = Files.createTempDirectory("track-monitor-watch-rescan")
    val musicFile = directory.resolve("Music.scala")
    val trackFile = directory.resolve("Bass0.scala")

    writeTrack(trackFile, 60)
    writeMusicCollectionFromTrackObjects(musicFile, List("Bass0"))

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

    try {
      monitor.scanReplaceAndResume.unsafeRunSync()
      IO.sleep(100.millis).unsafeRunSync()
      writeTrack(trackFile, 67)

      val updatedPlan = awaitReplacement(harness, 2, 10.seconds).unsafeRunSync()
      assertEquals(extractFirstNoteFromPlan(updatedPlan), 67)
    } finally monitor.stop.unsafeRunSync()
  }

  test("missing music file keeps current plan unchanged") {
    val directory = Files.createTempDirectory("track-monitor-music-missing")
    val musicFile = directory.resolve("Music.scala")

    writeTrack(directory.resolve("Bass0.scala"), 52)
    Files.writeString(
      musicFile,
      """object Music extends app.syntax.MusicFile {
        |  def play: Seq[app.domain.Track] = Seq(Bass0())
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

    val firstResult = monitor.scanReplaceAndResume.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(firstResult), 52)
    assertEquals(harness.replaceCalls.size, 1)

    Files.delete(musicFile)

    val secondResult = monitor.scanReplaceAndResume.unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(secondResult), 52)
    assertEquals(harness.replaceCalls.size, 1)
  }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case cats.data.Validated.Valid(value) => value
    case cats.data.Validated.Invalid(errors) => fail(s"Invalid test value: $errors")
  }

  private def writeTrack(path: Path, note: Int): Unit =
    Files.writeString(
      path,
      s"""object ${valid(classNameFromFilePath(path))} {
         |  given app.domain.Channel = app.domain.Channel.Channel0
         |  def apply(): app.domain.Track = app.domain.Track.track(
         |    timing = app.domain.Track.time(1),
         |    notes = app.domain.Track.note($note),
         |    duration = Some(app.domain.Track.duration(1)),
         |    velocity = app.domain.Track.velocity(app.domain.Velocity.Default)
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
      override def context: AnyRef               = Path.of("")
    }

  private def extractFirstNoteFromPlan(plan: PlaybackPlan): Int =
    planEvents(plan)
      .collectFirst { case TimedEvent(_, AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, note, _))) =>
        note.value
      }
      .getOrElse(fail("Missing NoteOn event in playback plan"))

  private def planEvents(plan: PlaybackPlan): Seq[TimedEvent] = plan match {
    case PlaybackPlan.EmptyPlaybackPlan              => Seq.empty
    case PlaybackPlan.PlaybackPlanData(events)       => events
  }

  private def awaitReplacement(
    harness: TrackDirectoryMonitorTestHarness,
    count: Int,
    remaining: FiniteDuration
  ): IO[PlaybackPlan] =
    IO.defer {
      harness.replaceCalls.lift(count - 1) match {
        case Some(plan)                         => IO.pure(plan)
        case None if remaining <= Duration.Zero => IO.raiseError(new AssertionError("playback was not replaced"))
        case None => IO.sleep(25.millis) *> awaitReplacement(harness, count, remaining - 25.millis)
      }
    }

  private final class TrackDirectoryMonitorTestHarness extends PlaybackController {
    private val replaceCallsRef = new AtomicReference[List[PlaybackPlan]](Nil)
    def replaceCalls: List[PlaybackPlan] = replaceCallsRef.get()

    override def play(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] = IO.unit
    override def pause: IO[Unit]                                          = IO.unit
    override def resume: IO[Unit]                                         = IO.unit
    override def stop: IO[Unit]                                           = IO.unit
    override def replace(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] = {
      replaceCallsRef.updateAndGet(calls => calls :+ plan)
      IO.unit
    }
    override def elapsedTime: IO[FiniteDuration]      = IO.pure(0.millis)
    override def status: IO[PlaybackStatus]           = IO.pure(PlaybackStatus.Stopped)
    override def activePlan: IO[Option[PlaybackPlan]] = IO.pure(replaceCalls.lastOption)
  }
}
