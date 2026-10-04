package app.playback

import app.domain._
import app.domain.given
import app.playback.TrackFileParser.classNameFromFilePath
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import munit.FunSuite

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.nio.file.WatchEvent
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

class TrackDirectoryMonitorSpec extends FunSuite {

  test("resumeFrom trims the plan from the current elapsed moment") {
    val plan = PlaybackPlan(
      Vector(
        TimedEvent(
          10.millis,
          AbsoluteMidiEvent(
            Tick.zero,
            MidiCommand
              .NoteOn(Channel.Ch0, valid(MidiValue[NoteTag](60)), valid(MidiValue[VelocityTag](100)))
          )
        ),
        TimedEvent(
          20.millis,
          AbsoluteMidiEvent(
            valid(Tick.fromInt(480)),
            MidiCommand.NoteOff(Channel.Ch0, valid(MidiValue[NoteTag](60)))
          )
        ),
        TimedEvent(
          5.millis,
          AbsoluteMidiEvent(
            valid(Tick.fromInt(960)),
            MidiCommand.NoteOn(
              Channel.Ch0,
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
    writeMusic(directory.resolve("Piano.scala"), 60)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce(false).unsafeRunSync()
    assertEquals(result.size, 2)
    assertEquals(harness.replaceCalls.size, 1)
  }

  test("thesis 1: without another scan after save, playback still uses old parsed track") {
    val directory = Files.createTempDirectory("track-monitor-thesis1")
    val trackFile = directory.resolve("Track1.scala")
    writeMusic(trackFile, 38)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    monitor.scanOnce(false).unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(harness.replaceCalls.last), 38)

    writeMusic(trackFile, 40)
    assertEquals(extractFirstNoteFromPlan(harness.replaceCalls.last), 38)
  }

  test("thesis 2: after save and rescan, parser reads updated file contents") {
    val directory = Files.createTempDirectory("track-monitor-thesis2")
    val trackFile = directory.resolve("Track1.scala")
    writeMusic(trackFile, 38)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    monitor.scanOnce(false).unsafeRunSync()
    writeMusic(trackFile, 40)
    monitor.scanOnce(false).unsafeRunSync()

    assertEquals(extractFirstNoteFromPlan(harness.replaceCalls.last), 40)
  }

  test("thesis 3: TrackFileCompiler returns updated note after file change") {
    val directory = Files.createTempDirectory("track-file-compiler-thesis3")
    val scalaFile = directory.resolve("Track1.scala")

    writeMusic(scalaFile, 38)
    val first = TrackFileParser.compileAndEvaluateFile(scalaFile)
    writeMusic(scalaFile, 40)
    val second = TrackFileParser.compileAndEvaluateFile(scalaFile)

    first match {
      case Valid(track)   => assertEquals(extractSingleNoteFromCompiledTrack(track), 38)
      case Invalid(error) => fail(s"first compilation failed: $error")
    }

    second match {
      case Valid(track)   => assertEquals(extractSingleNoteFromCompiledTrack(track), 40)
      case Invalid(error) => fail(s"second compilation failed: $error")
    }
  }

  test("music-file overrides parsed tracks when it returns non-empty list") {
    val directory = Files.createTempDirectory("track-monitor-music-override")
    val trackFile = directory.resolve("Track1.scala")
    val musicFile = Files.createTempFile("track-monitor-music-override-file", ".scala")

    writeMusic(trackFile, 38)
    writeMusicCollection(musicFile, List(64, 67))

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      musicFile = Some(musicFile),
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce(false).unsafeRunSync()

    assertEquals(extractFirstNoteFromPlan(result), 64)
    assertEquals(harness.replaceCalls.size, 1)
    assert(result.size >= 4)
  }

  test("music-file can reference track objects from the monitored directory") {
    val directory = Files.createTempDirectory("track-monitor-music-reference-tracks")
    val musicFile = Files.createTempFile("track-monitor-music-reference-tracks-file", ".scala")

    writeMusic(directory.resolve("Piano.scala"), 64)
    writeMusic(directory.resolve("Drums.scala"), 36)
    writeMusicCollectionFromTrackObjects(musicFile, List("Piano", "Drums"))

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      musicFile = Some(musicFile),
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce(false).unsafeRunSync()

    assertEquals(extractFirstNoteFromPlan(result), 64)
    assertEquals(harness.replaceCalls.size, 1)
    assert(result.size >= 4)
  }

  test("when music-file is valid monitor builds plan from music without per-track parsing") {
    val directory = Files.createTempDirectory("track-monitor-music-single-pass")
    val musicFile = Files.createTempFile("track-monitor-music-single-pass-file", ".scala")

    writeMusic(directory.resolve("Piano.scala"), 64)
    writeMusic(directory.resolve("Drums.scala"), 36)
    writeMusicCollectionFromTrackObjects(musicFile, List("Piano", "Drums"))

    val timing      = valid(TimingContext.from(480, 120))
    val harness     = new TrackDirectoryMonitorTestHarness()
    var parseCalls  = 0
    val countingParser: Path => cats.data.ValidatedNec[DomainError, Track] = _ => {
      parseCalls += 1
      Invalid(cats.data.NonEmptyChain.one(DomainError.EmptyTracks))
    }

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = countingParser,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      musicFile = Some(musicFile),
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce(false).unsafeRunSync()

    assertEquals(extractFirstNoteFromPlan(result), 64)
    assertEquals(parseCalls, 0)
    assertEquals(harness.replaceCalls.size, 1)
  }

  test("directory monitor excludes Music.scala from per-track parser input") {
    val directory = Files.createTempDirectory("track-monitor-excludes-music-definition")
    writeMusic(directory.resolve("Piano.scala"), 60)
    writeMusicCollectionFromTrackObjects(directory.resolve("Music.scala"), List("Piano"))

    val parsedFiles = ListBuffer.empty[String]
    val timing      = valid(TimingContext.from(480, 120))
    val harness     = new TrackDirectoryMonitorTestHarness()

    val parser: Path => cats.data.ValidatedNec[DomainError, Track] = path => {
      parsedFiles += path.getFileName.toString
      val track =
        Track.track(
          timeGen = Track.time(1),
          durGen = Track.duration(1),
          noteGen = Track.note(60)
        )(using Channel.Ch0)
      Valid(track)
    }

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = parser,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    monitor.scanOnce(false).unsafeRunSync()

    assertEquals(parsedFiles.toList.sorted, List("Piano.scala"))
  }

  test("replaceTracks is called only when resulting track list changes") {
    val directory = Files.createTempDirectory("track-monitor-diff-only")
    val trackFile = directory.resolve("Track1.scala")
    writeMusic(trackFile, 38)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    monitor.scanOnce().unsafeRunSync()
    monitor.scanOnce().unsafeRunSync()
    assertEquals(harness.replaceCalls.size, 0)

    writeMusic(trackFile, 40)
    monitor.scanOnce(false).unsafeRunSync()
    assertEquals(harness.replaceCalls.size, 1)
    assertEquals(extractFirstNoteFromPlan(harness.replaceCalls.last), 40)
  }

  test("scanOnce with default args returns plan but does not call replace") {
    val directory = Files.createTempDirectory("track-monitor-default-log-errors-only")
    writeMusic(directory.resolve("Track1.scala"), 52)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce().unsafeRunSync()
    assert(result.nonEmpty)
    assertEquals(extractFirstNoteFromPlan(result), 52)
    assertEquals(harness.replaceCalls.size, 0)
  }

  test("music-file does not override parsed tracks when it returns empty sequence") {
    val directory = Files.createTempDirectory("track-monitor-music-empty")
    val trackFile = directory.resolve("Track1.scala")
    val musicFile = Files.createTempFile("track-monitor-music-empty-file", ".scala")

    writeMusic(trackFile, 38)
    writeEmptyMusicCollection(musicFile)

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      musicFile = Some(musicFile),
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce(false).unsafeRunSync()
    assertEquals(extractFirstNoteFromPlan(result), 38)
    assertEquals(harness.replaceCalls.size, 1)
    assertEquals(extractFirstNoteFromPlan(harness.replaceCalls.last), 38)
  }

  test("scanOnce returns non-empty plan when one scala file fails but other tracks are valid") {
    val directory = Files.createTempDirectory("track-monitor-partial-parse-failure")
    writeMusic(directory.resolve("Piano.scala"), 60)
    writeMusic(directory.resolve("Drums.scala"), 36)

    Files.writeString(
      directory.resolve("Common.scala"),
      """object Common {
        |  val timeSeq: Seq[Double] = Seq(8.0, 8.0, 4.0)
        |}
        |""".stripMargin
    )

    val timing  = valid(TimingContext.from(480, 120))
    val harness = new TrackDirectoryMonitorTestHarness()

    val monitor = TrackDirectoryMonitor.live(
      directory = directory,
      parser = TrackFileParser.compileAndEvaluateFile,
      compiler = tracks => PlaybackPlan.fromTracks(tracks, timing),
      playback = harness,
      timing = timing,
      policy = RepeatPolicy.none,
      pollInterval = 10.millis
    )

    val result = monitor.scanOnce(false).unsafeRunSync()

    assert(result.nonEmpty)
    assertEquals(harness.replaceCalls.size, 1)
    assert(result.size >= 4)
  }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A = validated match {
    case Valid(value) => value
    case Invalid(_)   => throw new IllegalStateException("invalid test value")
  }

  private def writeMusic(path: Path, note: Int): Unit =
    Files.writeString(
      path,
      s"""object ${classNameFromFilePath(path)} extends app.syntax.TrackFile {
         |  given app.domain.Channel = app.domain.Channel.Ch0
         |  def apply(): app.domain.Track = app.domain.Track.track(
         |    timeGen = app.domain.Track.time(1),
         |    durGen = app.domain.Track.duration(1),
         |    noteGen = app.domain.Track.note($note)
         |  )
         |}
         |""".stripMargin
    )

  private def writeMusicCollection(path: Path, notes: List[Int]): Unit = {
    val noteTracks = notes
      .map { note =>
        s"app.domain.Track.track(timeGen = app.domain.Track.time(1), durGen = app.domain.Track.duration(1), noteGen = app.domain.Track.note($note))"
      }
      .mkString(",\n      ")

    Files.writeString(
      path,
      s"""object Music {
        |  given app.domain.Channel = app.domain.Channel.Ch0
        |  def music: Option[Seq[app.domain.Track]] = Some(Seq(
         |      $noteTracks
         |  ))
         |}
         |""".stripMargin
    )
  }

  private def writeEmptyMusicCollection(path: Path): Unit =
    Files.writeString(
      path,
      s"""object Music {
         |  given app.domain.Channel = app.domain.Channel.Ch0
         |  def music: Option[Seq[app.domain.Track]] = Some(Seq.empty)
         |}
         |""".stripMargin
    )

  private def writeMusicCollectionFromTrackObjects(path: Path, objects: List[String]): Unit = {
    val tracks = objects.map(name => s"$name()").mkString(",\n      ")

    Files.writeString(
      path,
      s"""object Music {
         |  def music: Option[Seq[app.domain.Track]] = Some(Seq(
         |      $tracks
         |  ))
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

  private def extractSingleNote(track: Track): Int =
    track.noteGen match {
      case Generator.NoteGen(notes) =>
        notes.headOption.getOrElse(throw new IllegalStateException("missing note in test track"))
      case _ =>
        throw new IllegalStateException("unexpected generator kind in test track")
    }

  private def extractSingleNoteFromCompiledTrack(track: Track): Int =
    extractSingleNote(track)

  private def extractFirstNoteFromPlan(plan: PlaybackPlan): Int =
    plan.events
      .collectFirst { case TimedEvent(_, AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, note, _))) =>
        note.value
      }
      .getOrElse(throw new IllegalStateException("missing NoteOn event in playback plan"))

  private final class TrackDirectoryMonitorTestHarness extends PlaybackController {
    var playCalls: List[PlaybackPlan]    = Nil
    var replaceCalls: List[PlaybackPlan] = Nil

    override def play(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] = {
      playCalls = playCalls :+ plan
      IO.unit
    }

    override def pause: IO[Unit]  = IO.unit
    override def resume: IO[Unit] = IO.unit
    override def stop: IO[Unit]   = IO.unit
    override def replace(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] = {
      replaceCalls = replaceCalls :+ plan
      IO.unit
    }

    override def elapsedTime: IO[FiniteDuration] = IO.pure(0.millis)
  }
}
