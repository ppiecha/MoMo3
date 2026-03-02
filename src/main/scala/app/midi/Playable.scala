package app.midi

import scala.concurrent.duration.FiniteDuration
import javax.sound.midi.Sequence
import cats.effect.*
import app.*
import app.midi.ReactiveSynth

trait Playable {
  def midiStreams[F[_]](duration: Option[FiniteDuration]): App[F, List[MidiStream[F]]]

  def play[F[_]: Async](duration: Option[FiniteDuration] = None): App[F, Unit] =
    for {
      env    <- ask[F]
      streams <- midiStreams(duration)
    } yield ReactiveSynth.resource[F](streams).use { case (_, all) => all.compile.drain }
}
