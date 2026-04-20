package app.application

import cats.effect.*
import fs2.*

import app.config.{DomainException, Environment}
import app.midi.ReactiveSynth
import javax.sound.midi.ShortMessage

object PlaybackService {

  def compiledTrackToStream[F[_]: Async](compiledTrack: CompiledTrack): Stream[F, Stream[F, ShortMessage]] = {
    Stream
      .emits(compiledTrack.events)
      .flatMap{
        case Left(domainError) => Stream.raiseError[F](DomainException(domainError))
        case Right(event) => Stream(event.streamOfMidiMessages[F]) ++ Stream.sleep_[F](event.time.duration)
      }
  }

  def play[F[_]: Async](compiledTracks: List[CompiledTrack], env: Environment): F[Unit] = {
    val midiStreams = compiledTracks.map(compiledTrackToStream)
    ReactiveSynth.resource(midiStreams, env).use { case (_, all) =>
      all.compile.drain
    }
  }
}
