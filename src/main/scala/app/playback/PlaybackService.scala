package app.playback

import app.domain.*
import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import fs2.*

object PlaybackService {
  def executePlaybackPlan[F[_]: Temporal](
    playbackPlan: PlaybackPlan,
    send: AbsoluteMidiEvent => F[Unit]
  ): F[Either[DomainError, Unit]] = {
    Stream
      .emits(playbackPlan.events)
      .evalMap { case TimedEvent(delay, event) =>
        Temporal[F].sleep(delay) *> send(event)
      }
      .compile
      .drain
      .as(Right(()))
  }
}
