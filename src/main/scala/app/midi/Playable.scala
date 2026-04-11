package app.midi

import scala.concurrent.duration.FiniteDuration
import javax.sound.midi.Sequence
import cats.effect.*
import app.*
import app.midi.ReactiveSynth
import fs2.*
import javax.sound.midi.ShortMessage

trait Playable {
  def midiStreams[F[_]: Async]: App[F, List[Stream[F, Stream[F, ShortMessage]]]]

  def play[F[_]: Async](duration: Option[FiniteDuration] = None): App[F, Unit] =
    for {
      env     <- ask
      streams <- midiStreams
    } yield ReactiveSynth.resource[F](streams, env).use { case (_, all) =>
      all.compile.drain
    }
}
