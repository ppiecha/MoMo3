package app.application

import cats.effect.*
import fs2.*

import app.*
import app.midi.ReactiveSynth

object PlaybackService {
  def play[F[_]: Async](compiledTracks: List[CompiledTrack], env: Environment): F[Unit] = {
    val midiStreams = compiledTracks.map { ct =>
      Stream
        .emits(ct.events)
        .flatMap(event => Stream(event.streamOfMidiMessages[F]) ++ Stream.sleep_[F](event.time.duration))
    }
    ReactiveSynth.resource(midiStreams, env).use { case (_, all) =>
      all.compile.drain
    }
  }
}
