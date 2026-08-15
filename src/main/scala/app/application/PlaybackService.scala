package app.application

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import app.config.{Environment, TimingContext}
import app.domain.*

import scala.concurrent.duration.*

object PlaybackService {

  case class TimedEvent(delay: FiniteDuration, event: AbsoluteMidiEvent)
  case class PlaybackPlan(events: Seq[TimedEvent])

  def compiledTrackToAbsoluteEvents(compiledTrack: CompiledTrack): Either[DomainError, Seq[AbsoluteMidiEvent]] =
    compiledTrack.events.sequence

  def toTimedEvents(
    events: Seq[AbsoluteMidiEvent],
    timingContext: TimingContext
  ): Seq[TimedEvent] = {
    events
      .sortBy(_.at.value)
      .foldLeft((Tick.zero, Vector.empty[(Tick, AbsoluteMidiEvent)])) { case ((prev, acc), e) =>
        val delta = e.at - prev
        (e.at, acc :+ (delta -> e))
      }
      ._2
      .map { case (tick, event) => TimedEvent(tick.toMillis(timingContext.ppq, timingContext.bpm), event) }
  }

  def toPlaybackPlan(
    compiledTracks: Seq[CompiledTrack],
    timingContext: TimingContext
  ): Either[DomainError, PlaybackPlan] =
    compiledTracks
      .traverse(compiledTrackToAbsoluteEvents)
      .map(_.flatten)
      .map(events => PlaybackPlan(toTimedEvents(events, timingContext)))

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
