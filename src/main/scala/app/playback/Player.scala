package app.playback

import app.domain.{AbsoluteMidiEvent, DomainError, PlaybackPlan, TimingContext, Track}
import cats.data.EitherT
import cats.effect.Temporal

final class Player[F[_]: Temporal](
  pipeline: PlaybackPipeline,
  send: AbsoluteMidiEvent => F[Unit]
) {
  def play(tracks: List[Track], timing: TimingContext): F[Either[DomainError, Unit]] =
    pipeline.build(tracks, timing) match {
      case Left(err)   => Temporal[F].pure(Left(err))
      case Right(plan) => play(plan)
    }

  private def play(plan: PlaybackPlan): F[Either[DomainError, Unit]] =
    PlaybackService.executePlaybackPlan(plan, send)
}

object Player {
  def live[F[_]: Temporal](send: AbsoluteMidiEvent => F[Unit]): Player[F] =
    new Player[F](PlaybackPipeline.live, send)
}
