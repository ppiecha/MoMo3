package app.playback

import app.domain.{AbsoluteMidiEvent, DomainError, PlaybackPlan, TimingContext, Track}
import cats.effect.Temporal
import cats.syntax.all.*

final class Player[F[_]: Temporal](
  pipeline: PlaybackPipeline,
  send: AbsoluteMidiEvent => F[Unit]
) {
  def play(tracks: List[Track], timing: TimingContext): F[Either[DomainError, Unit]] =
    pipeline.build(tracks, timing).fold(err => Temporal[F].pure(Left(err)), plan => play(plan).map(Right(_)))

  private def play(plan: PlaybackPlan): F[Unit] =
    PlaybackService.executePlaybackPlan(plan, send)
}

object Player {
  def live[F[_]: Temporal](send: AbsoluteMidiEvent => F[Unit]): Player[F] =
    new Player[F](PlaybackPipeline.live, send)
}
