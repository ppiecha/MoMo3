package app.midi

import scala.concurrent.duration.FiniteDuration
import javax.sound.midi.Sequence
import cats.effect.*
import app.*
import app.midi.ReactiveSynth
import fs2.*
import javax.sound.midi.MidiMessage

trait Playable {
  def midiStreams[F[_]: Async]: App[F, List[Stream[F, Stream[F, MidiMessage]]]]

  def play[F[_]: Async](duration: Option[FiniteDuration] = None): App[F, Unit] =
    for {
      env     <- ask
      streams <- midiStreams
    } yield ReactiveSynth.resource[F](streams, env.soundFontPath).use { case (_, all) => all.compile.drain }
}
