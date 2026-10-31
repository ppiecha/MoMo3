package app.playback

import app.domain.AbstractMidiEvent
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.domain.Channel
import app.domain.MidiCommand
import app.domain.PlaybackPlan
import app.domain.Tick
import app.domain.TimedEvent
import cats.Applicative
import cats.effect.Temporal
import cats.syntax.all.*

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.MILLISECONDS

/** Executes a playback plan while reporting the elapsed time after every event. This is used by the controller to
  * pause/resume at the current position.
  */
object PlaybackExecution {

  def sendAllNotesOff[F[_]: Applicative](send: AbstractMidiEvent => F[Unit]): F[Unit] =
    sendToAllChannels(send, MidiCommand.AllNotesOff.apply)

  def sendAllSoundsOff[F[_]: Applicative](send: AbstractMidiEvent => F[Unit]): F[Unit] =
    sendToAllChannels(send, MidiCommand.AllSoundsOff.apply)

  private def sendToAllChannels[F[_]: Applicative](
    send: AbstractMidiEvent => F[Unit],
    command: Channel => MidiCommand
  ): F[Unit] =
    Channel.values.toList.traverse_(channel => send(AbsoluteMidiEvent(Tick.Zero, command(channel))))

  def executeWithProgress[F[_]: Temporal](
    plan: PlaybackPlan,
    send: AbstractMidiEvent => F[Unit],
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
    plan match {
      case PlaybackPlan.EmptyPlaybackPlan => Temporal[F].pure(FiniteDuration(0L, MILLISECONDS))
      case PlaybackPlan.PlaybackPlanData(events) =>
        loop(events.toVector, FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS))
    }
  }

  def executeWithProgress[F[_]: Temporal](
    plan: PlaybackPlan,
    send: AbstractMidiEvent => F[Unit]
  ): F[FiniteDuration] =
    executeWithProgress(plan, send, _ => Temporal[F].unit)
}
