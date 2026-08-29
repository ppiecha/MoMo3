package app.playback

import app.domain.{AbsoluteMidiEvent, PlaybackPlan, TimedEvent}
import cats.effect.Temporal
import cats.syntax.all.*

import scala.concurrent.duration.FiniteDuration

/** Executes a playback plan while reporting the elapsed time after every event. This is used by the controller to
  * pause/resume at the current position.
  */
object PlaybackExecution {

  def executeWithProgress[F[_]: Temporal](
    plan: PlaybackPlan,
    send: AbsoluteMidiEvent => F[Unit],
    onProgress: FiniteDuration => F[Unit]
  ): F[FiniteDuration] = {
    def loop(events: Vector[TimedEvent], elapsed: FiniteDuration): F[FiniteDuration] =
      events match {
        case Vector() => Temporal[F].pure(elapsed)
        case head +: tail =>
          Temporal[F].sleep(head.delay) *>
            send(head.event) *>
            onProgress(elapsed + head.delay) *>
            loop(tail, elapsed + head.delay)
      }

    loop(plan.events.toVector, FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS))
  }

  def executeWithProgress[F[_]: Temporal](
    plan: PlaybackPlan,
    send: AbsoluteMidiEvent => F[Unit]
  ): F[FiniteDuration] =
    executeWithProgress(plan, send, _ => Temporal[F].unit)
}
