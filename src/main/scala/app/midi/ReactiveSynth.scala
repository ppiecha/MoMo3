import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2.Stream
import javax.sound.midi._

object ReactiveSynth {

  def resource[F[_]: Async](inputs: List[Stream[F, MidiMessage]]): Resource[F, (List[Queue[F, Option[MidiMessage]]], Stream[F, Unit])] =
    for {
      synth    <- Resource.make(Async[F].delay(MidiSystem.getSynthesizer))(s => Async[F].delay(s.close()))
      _        <- Resource.eval(Async[F].delay(synth.open()))
      receiver <- Resource.make(Async[F].delay(synth.getReceiver))(r => Async[F].delay(r.close()))
      queues   <- Resource.eval(inputs.traverse(_ => Queue.unbounded[F, Option[MidiMessage]]))
    } yield {
      // Each stream puts messages into its own queue
      val inputStreams = inputs.zip(queues).map { case (stream, queue) =>
        stream.evalMap(msg => Async[F].delay(println(s"Sending message $msg to queue $queue")) *> queue.offer(Some(msg)))
      }
      // Queue streams consume messages and send them to the synthesizer
      val midiStreams = queues.map { queue =>
        Stream.fromQueueNoneTerminated(queue).evalMap(msg => Async[F].delay { receiver.send(msg, -1) })
      }
      // Start all streams (can be merged or run in parallel)
      val all = Stream(inputStreams: _*).parJoinUnbounded.merge(Stream(midiStreams: _*).parJoinUnbounded)
      // You can return just the queues or a tuple (queues, all)
      (queues, all)
    }
}
