package app.playback

import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.syntax.Extensions.*
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import munit.FunSuite

class SectionConcatenationSpec extends FunSuite {
  private given TimingContext = TimingContext.from(960, 120) match {
    case Valid(context)  => context
    case Invalid(errors) => fail(s"Invalid timing context: $errors")
  }

  private def track(channel: Channel, timing: TimeGen, notes: Generator[Note]): Track =
    Track.track(timing, notes)(using channel, Some(Instrument(0, 20)))

  private def noteOns(section: Section): List[(Channel, Int, Int)] =
    PlaybackPlan.fromTracks(section).right.getOrElse(fail("Expected a valid playback plan")) match {
      case PlaybackPlan.PlaybackPlanData(events, _) =>
        events.toList.collect { case TimedEvent(_, AbsoluteMidiEvent(at, MidiCommand.NoteOn(channel, note, _))) =>
          (channel, at.value, note.value)
        }
      case PlaybackPlan.EmptyPlaybackPlan => Nil
    }

  test("sections join tracks by channel and insert rests for missing channels") {
    val left = Section(
      track(Channel.Channel0, Track.time(4, 4), Track.note(60, 62)),
      track(Channel.Channel1, Track.time(2), Track.note(64))
    )
    val right = Section(
      track(Channel.Channel2, Track.time(8, 8, 4), Track.note(67, 69, 71)),
      track(Channel.Channel0, Track.time(2), Track.note(65))
    )

    val joined = left ++ right
    val tracks = joined.toList
    assertEquals(tracks.map(_.channel), List(Channel.Channel0, Channel.Channel1, Channel.Channel2))
    assert(tracks.forall(_.timing.duration == 1.0))
    assert(tracks.forall(_.instrument.contains(Instrument(0, 20))))
    assertEquals(
      noteOns(joined).toSet,
      Set(
        (Channel.Channel0, 0, 60),
        (Channel.Channel0, 960, 62),
        (Channel.Channel0, 1920, 65),
        (Channel.Channel1, 0, 64),
        (Channel.Channel2, 1920, 67),
        (Channel.Channel2, 2400, 69),
        (Channel.Channel2, 2880, 71)
      )
    )
  }

  test("disjoint sections retain leading and trailing silence across repeated concatenations") {
    val first  = Section(track(Channel.Channel0, Track.time(4), Track.note(60)))
    val second = Section(track(Channel.Channel1, Track.time(2), Track.note(62)))
    val joined = first ++ second ++ first
    assertEquals(joined.toList.map(_.timing.duration), List(1.0, 1.0))
    assertEquals(
      noteOns(joined).toSet,
      Set((Channel.Channel0, 0, 60), (Channel.Channel1, 960, 62), (Channel.Channel0, 2880, 60))
    )
    assertEquals(noteOns(first.repeat(2)).map(_._3), List(60, 60))
  }

  test("muted tracks stay silent even when the channel is absent from the other section") {
    val muted  = Section(track(Channel.Channel0, Track.time(4), Track.note(60)).copy(muted = true))
    val active = Section(track(Channel.Channel1, Track.time(4), Track.note(62)))
    assert((muted ++ active).toList.exists(track => track.channel == Channel.Channel0 && track.muted))
    assertEquals(noteOns(muted ++ active), List((Channel.Channel1, 960, 62)))
    assertEquals(noteOns(active ++ muted), List((Channel.Channel1, 0, 62)))
  }

  test("repeated channels pair by instrument and pad unmatched tracks on both sides") {
    val base = track(Channel.Channel0, Track.time(4), Track.note(60))
    val left = Section(
      base.copy(instrument = Some(Instrument(0, 10))),
      base.copy(notes = Some(Track.note(61)), instrument = Some(Instrument(0, 20))),
      base.copy(notes = Some(Track.note(62)), instrument = None)
    )
    val right = Section(
      base.copy(notes = Some(Track.note(71)), instrument = Some(Instrument(0, 30))),
      base.copy(notes = Some(Track.note(70)), instrument = Some(Instrument(0, 20))),
      base.copy(notes = Some(Track.note(72)), instrument = None)
    )
    val joined = left ++ right
    assertEquals(joined.toList.length, 4)
    assertEquals(
      joined.toList.map(track => (track.instrument, track.notes)).toSet,
      Set(
        (Some(Instrument(0, 10)), Some(Track.note(60, 0))),
        (Some(Instrument(0, 20)), Some(Track.note(61, 70))),
        (None, Some(Track.note(62, 72))),
        (Some(Instrument(0, 30)), Some(Track.note(0, 71)))
      )
    )
    assertEquals(noteOns(joined).map(_._3).sorted, List(60, 61, 62, 70, 71, 72))
    assert(joined.toList.forall(_.timing.duration == 0.5))
  }

  test("duplicate channel and instrument tracks are paired without reuse, including unequal counts") {
    val base  = track(Channel.Channel0, Track.time(4), Track.note(60))
    val left  = Section(base, base.copy(notes = Some(Track.note(61))), base.copy(notes = Some(Track.note(62))))
    val right = Section(base.copy(notes = Some(Track.note(70))), base.copy(notes = Some(Track.note(71))))

    List.fill(20)(left ++ right).foreach { joined =>
      assertEquals(joined.toList.length, 3)
      val events = noteOns(joined)
      assertEquals(events.map(_._3).sorted, List(60, 61, 62, 70, 71))
      assertEquals(events.count(_._2 == 0), 3)
      assertEquals(events.count(_._2 == 960), 2)
    }
    val reverse = right ++ left
    assertEquals(reverse.toList.length, 3)
    assertEquals(noteOns(reverse).map(_._3).sorted, List(60, 61, 62, 70, 71))
  }

  test("a channel occurring once on each side still joins even if instruments differ") {
    val one    = track(Channel.Channel0, Track.time(4), Track.note(60))
    val other  = one.copy(instrument = Some(Instrument(0, 30)), notes = Some(Track.note(70)))
    val joined = Section(one) ++ Section(other)
    assertEquals(joined.toList.length, 1)
    assertEquals(noteOns(joined).map(_._3), List(60, 70))
  }

  test("rejects unequal durations in either section") {
    val one        = track(Channel.Channel0, Track.time(4), Track.note(60))
    val other      = track(Channel.Channel1, Track.time(2), Track.note(64))
    val valid      = Section(one)
    val mismatched = Section(one, other)

    intercept[IllegalArgumentException](mismatched ++ valid)
    intercept[IllegalArgumentException](valid ++ mismatched)
  }
}
