package app.domain

import app.playback.TrackCompiler
import cats.data.ValidatedNec
import cats.syntax.all.*

case class PlaybackPlan(events: Seq[TimedEvent]) {
  def isEmpty: Boolean  = events.isEmpty
  def nonEmpty: Boolean = events.nonEmpty
  def size: Int         = events.size
}

object PlaybackPlan {

  def empty: PlaybackPlan = PlaybackPlan(Seq.empty)

  def fromTracks(
    tracks: Tracks,
    timingContext: TimingContext
  ): ValidatedNec[ValidationError, PlaybackPlan] =
    tracks.toSeq
      .traverse(track => TrackCompiler.compile(track, timingContext))
      .map(_.flatten)
      .map(events => PlaybackPlan(TimedEvent.fromAbsoluteEvents(events, timingContext)))
}
