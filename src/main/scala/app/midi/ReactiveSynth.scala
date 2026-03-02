package app.midi

import cats.data.*
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2.Stream
import javax.sound.midi._
import app.*
import cats.MonadThrow
import java.nio.file.Paths

def test[F[_]: Async](i: Int): App[F, Int] = EitherT.rightT(i)

object ReactiveSynth {

  def loadSynthesizer[F[_]: Async: MonadThrow](soundFontPath: String): F[Synthesizer] =
    for {
      synth     <- Async[F].delay(MidiSystem.getSynthesizer)
      sfFile    <- Async[F].blocking(Paths.get(soundFontPath).toFile)
      soundBank <- Async[F].delay(MidiSystem.getSoundbank(sfFile))
      supported <- Async[F].delay(synth.isSoundbankSupported(soundBank))
      _ <-
        if !supported then Async[F].unit
        else MonadThrow[F].raiseError(new Exception(s"Soundbank not supported: $soundFontPath"))
      loaded <- Async[F].delay(synth.loadAllInstruments(soundBank))
      _ <-
        if !loaded then Async[F].unit
        else MonadThrow[F].raiseError(new Exception(s"Failed to load soundbank: $soundFontPath"))
    } yield synth

  def resource[F[_]: Async](
      inputs: List[MidiStream[F]]
  ): Resource[F, (List[Queue[F, Option[MidiMessage]]], Stream[F, Unit])] =
    for {
      synth    <- Resource.make(Async[F].delay(MidiSystem.getSynthesizer))(s => Async[F].delay(s.close()))
      _        <- Resource.eval(Async[F].delay(synth.open()))
      receiver <- Resource.make(Async[F].delay(synth.getReceiver))(r => Async[F].delay(r.close()))
      queues   <- Resource.eval(inputs.traverse(_ => Queue.unbounded[F, Option[MidiMessage]]))
    } yield {
      // Each stream puts messages into its own queue
      val inputStreams = inputs.zip(queues).map { case (stream, queue) =>
        stream.evalMap(msg => queue.offer(Some(msg)))
      }
      // Queue streams consume messages and send them to the synthesizer
      val midiStreams = queues.map { queue =>
        Stream.fromQueueNoneTerminated(queue).evalMap(msg => Async[F].delay { receiver.send(msg, -1) })
      }
      // Start all streams (can be merged or run in parallel)
      val all = Stream(inputStreams: _*).parJoinUnbounded.merge(Stream(midiStreams: _*).parJoinUnbounded)
      (queues, all)
    }
}
