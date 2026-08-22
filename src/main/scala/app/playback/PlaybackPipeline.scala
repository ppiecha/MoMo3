package app.playback

import app.domain.{DomainError, PlaybackPlan, TimingContext, Track}

trait PlaybackPipeline {
  def build(tracks: List[Track], timing: TimingContext): Either[DomainError, PlaybackPlan]
}

object PlaybackPipeline {
  val live: PlaybackPipeline = new PlaybackPipeline {
    override def build(tracks: List[Track], timing: TimingContext): Either[DomainError, PlaybackPlan] =
      PlaybackPlan.fromCompiledTracks(
        tracks.map(track => app.application.TrackCompiler.compile(track, timing)),
        timing
      )
  }
}
