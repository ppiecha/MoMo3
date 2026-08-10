package app.application

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import app.config.{Environment, TimingContext}
import app.domain.*

import scala.concurrent.duration.*

object PlaybackService {

  def compiledTrackToAbsoluteEvents(compiledTrack: CompiledTrack): Either[DomainError, List[AbsoluteMidiEvent]] =
    compiledTrack.events.toList.sequence

  def toPlaybackPlan(
    compiledTracks: List[CompiledTrack]
  ): Either[DomainError, List[AbsoluteMidiEvent]] =
    compiledTracks.traverse(compiledTrackToAbsoluteEvents).map(_.flatten)

  def play[F[_]: Temporal](
    compiledTracks: List[CompiledTrack],
    timingContext: TimingContext,
    send: AbsoluteMidiEvent => F[Unit]
  ): F[Either[DomainError, Unit]] = {

    // 1. Track conversion → absolute events
    val absoluteEventsEither: Either[DomainError, List[AbsoluteMidiEvent]] =
      compiledTracks
        .traverse(compiledTrackToAbsoluteEvents)
        .map(_.flatten)

    // 2. Absolute events conversion → timed events
    val timedEventsEither: Either[DomainError, List[(FiniteDuration, AbsoluteMidiEvent)]] =
      absoluteEventsEither.map(events => toTimedEvents(events, timingContext))

    // 3. Lifting Either to F, without exceptions
    timedEventsEither match {
      case Left(err) =>
        // return error as a value, not an exception
        Temporal[F].pure(Left(err))

      case Right(events) =>
        // 4. Real-time playback of events
        Stream
          .emits(events)
          .evalMap { case (delay, event) =>
            Temporal[F].sleep(delay) *> send(event)
          }
          .compile
          .drain
          .as(Right(()))
    }
  }

  private def toTimedEvents(
    events: List[AbsoluteMidiEvent],
    timingContext: TimingContext
  ): List[(FiniteDuration, AbsoluteMidiEvent)] = {
    events
      .sortBy(_.at.value)
      .foldLeft((Tick.zero, List.empty[(Tick, AbsoluteMidiEvent)])) { case ((prev, acc), e) =>
        val delta = e.at - prev
        (e.at, acc :+ (delta -> e))
      }
      ._2
      .map { case (tick, event) => (tick.toMillis(timingContext.ppq, timingContext.bpm), event) }
  }
}
