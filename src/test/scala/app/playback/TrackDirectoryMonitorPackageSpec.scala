package app.playback

import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite

import java.nio.file.Files
import java.util.Comparator
import scala.concurrent.duration.*

class TrackDirectoryMonitorPackageSpec extends FunSuite {
  test("monitor compiles and plays packaged music and track sources") {
    val directory = Files.createTempDirectory("track-monitor-packaged")
    val musicFile = directory.resolve("Music.scala")
    val trackFile = directory.resolve("Bass0.scala")
    Files.writeString(
      musicFile,
      """package test1
        |object Music extends app.syntax.MusicFile {
        |  def play: app.domain.Section = app.domain.Section.of(Bass0.track)
        |}
        |""".stripMargin
    )
    Files.writeString(
      trackFile,
      """package test1
        |object Bass0 {
        |  given app.domain.Channel = app.domain.Channel.Channel0
        |  given Option[app.domain.Instrument] = None
        |  def track: app.domain.Track = app.domain.Track.track(
        |    timing = app.domain.Track.time(1),
        |    notes = app.domain.Track.note(72)
        |  )
        |}
        |""".stripMargin
    )

    val timing = TimingContext.from(480, 120) match {
      case cats.data.Validated.Valid(value)    => value
      case cats.data.Validated.Invalid(errors) => fail(s"Invalid timing: $errors")
    }
    val playback = new PlaybackController {
      def play(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit]    = IO.unit
      def pause: IO[Unit]                                             = IO.unit
      def resume: IO[Unit]                                            = IO.unit
      def stop: IO[Unit]                                              = IO.unit
      def replace(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] = IO.unit
      def elapsedTime: IO[FiniteDuration]                             = IO.pure(Duration.Zero)
      def status: IO[PlaybackStatus]                                  = IO.pure(PlaybackStatus.Stopped)
      def activePlan: IO[Option[PlaybackPlan]]                        = IO.pure(None)
    }
    val monitor = TrackDirectoryMonitor
      .live(directory, playback, timing, musicFile, RepeatPolicy.none, 10.millis)
      .unsafeRunSync()

    try {
      val plan = monitor.scanAndPlay.unsafeRunSync()
      val notes = plan match {
        case PlaybackPlan.EmptyPlaybackPlan => Seq.empty
        case PlaybackPlan.PlaybackPlanData(events) =>
          events.collect { case TimedEvent(_, AbsoluteMidiEvent(_, MidiCommand.NoteOn(_, note, _))) =>
            note.value
          }
      }
      assertEquals(notes, Seq(72))
    } finally {
      monitor.stop.unsafeRunSync()
      val paths = Files.walk(directory)
      try paths.sorted(Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
      finally paths.close()
    }
  }
}
