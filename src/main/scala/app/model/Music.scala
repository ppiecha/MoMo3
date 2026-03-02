package app.model

import cats.data.Kleisli
import cats.syntax.all.*
import app.*
import scala.concurrent.duration.FiniteDuration
import javax.sound.midi.Sequence
import app.midi.Playable

case class Music(tracks: List[Track]) extends Playable {
  override def sequence(duration: Option[FiniteDuration]): App[Sequence] =
    for {
      env <- Kleisli.ask[ErrorOr, Env]
      emptySequence = new Sequence(Sequence.PPQ, env.ctx.ppq.value)
      sequence <- tracks.foldM(emptySequence)((seq, track) => Track.addTrackToSequence(track, seq, duration))
    } yield sequence
}
