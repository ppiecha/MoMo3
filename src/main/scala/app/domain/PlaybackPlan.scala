package app.domain

import cats.syntax.all.*
import app.application.CompiledTrack

case class PlaybackPlan(events: Seq[TimedEvent])

object PlaybackPlan {
  def compiledTrackToAbsoluteEvents(compiledTrack: CompiledTrack): Either[DomainError, Seq[AbsoluteMidiEvent]] =
    compiledTrack.events.sequence

  def fromCompiledTracks(
    compiledTracks: Seq[CompiledTrack],
    timingContext: TimingContext
  ): Either[DomainError, PlaybackPlan] =
    compiledTracks
      .traverse(compiledTrackToAbsoluteEvents)
      .map(_.flatten)
      .map(events => PlaybackPlan(TimedEvent.fromAbsoluteEvents(events, timingContext)))
}
