package app.domain

import app.playback.TrackCompiler
import app.syntax.toIorNec
import cats.data.IorNec
import cats.syntax.all.*

sealed trait PlaybackPlan
object PlaybackPlan {
  case class PlaybackPlanData(events: Seq[TimedEvent]) extends PlaybackPlan
  case object EmptyPlaybackPlan                        extends PlaybackPlan

  def fromTracks(tracks: Tracks)(using timingContext: TimingContext): IorNec[DomainError, PlaybackPlan] =
    tracks.toSeq
      .traverse(track => toIorNec(TrackCompiler.compile(track)))
      .map(_.flatten)
      .map(events => PlaybackPlanData(TimedEvent.fromAbsoluteEvents(events)))
}
