package app.domain

import app.playback.TrackCompiler
import app.syntax.toIorNec
import cats.data.IorNec
import cats.syntax.all._

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
  ): IorNec[DomainError, PlaybackPlan] =
    tracks.toSeq
      .traverse(track => toIorNec(TrackCompiler.compile(track, timingContext)))
      .map(_.flatten)
      .map(events => PlaybackPlan(TimedEvent.fromAbsoluteEvents(events, timingContext)))
}
