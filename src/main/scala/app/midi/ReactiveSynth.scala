import cats.effect._
import cats.effect.std.{Dispatcher, Queue}
import cats.syntax.all._
import fs2.Stream
import javax.sound.midi._

object ReactiveSynth {

  def resource[F[_]: Async]: Resource[F, (Queue[F, MidiMessage], Stream[F, Unit])] =
    for {
      synth <- Resource.make(
        Async[F].delay(MidiSystem.getSynthesizer)
      )(s => Async[F].delay(s.close()))

      _ <- Resource.eval(Async[F].delay(synth.open()))

      receiver <- Resource.make(
        Async[F].delay(synth.getReceiver)
      )(r => Async[F].delay(r.close()))

      queue <- Resource.eval(Queue.unbounded[F, MidiMessage])

    } yield {

      val stream =
        Stream
          .fromQueueUnterminated(queue)
          .evalMap(msg =>
            Async[F].delay {
              receiver.send(msg, -1) // -1 = send immediately
            }
          )

      (queue, stream)
    }
}