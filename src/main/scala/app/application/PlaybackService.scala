package app.application

import cats.effect.*
import cats.syntax.all.*
import fs2.*

import app.config.{DomainException, Environment}
import app.midi.ReactiveSynth
import app.domain.*
import app.shared.ErrorOr
import scala.concurrent.duration.*

object PlaybackService {

  def compiledTrackToAbsoluteEvents(compiledTrack: CompiledTrack): ErrorOr[List[AbsoluteMidiEvent]] =
    compiledTrack.events.toList.sequence

  def toPlaybackPlan(
      compiledTracks: List[CompiledTrack]
  ): ErrorOr[List[AbsoluteMidiEvent]] =
    compiledTracks.traverse(compiledTrackToAbsoluteEvents).map(_.flatten)

  def play[F[_]: Temporal](
      compiledTracks: List[CompiledTrack],
      env: Environment,
      send: AbsoluteMidiEvent => F[Unit]
  ): F[Unit] =
    compiledTracks
      .traverse(compiledTrackToAbsoluteEvents)
      .map(_.flatten)
      .map(toTimedEvents(_, env))
      .leftMap(DomainException.apply)
      .liftTo[F]
      .flatMap { events =>
        Stream
          .emits(events)
          .evalMap { case (delay, event) =>
            Temporal[F].sleep(delay) *> send(event)
          }
          .compile
          .drain
      }

  private def toTimedEvents(
      events: List[AbsoluteMidiEvent],
      env: Environment
  ): List[(FiniteDuration, AbsoluteMidiEvent)] =
    events
      .sortBy(_.at.value)
      .foldLeft((Tick.zero, List.empty[(Tick, AbsoluteMidiEvent)])) { case ((prev, acc), e) =>
        val delta = e.at - prev
        (e.at, acc :+ (delta -> e))
      }
      ._2
      .map { case (tick, event) => (tick.toMillis(env), event) }
}
