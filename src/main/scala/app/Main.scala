import cats.effect._
import fs2.Stream
import javax.sound.midi._

object Main extends IOApp.Simple {

  def noteOn(note: Int): MidiMessage = {
    val msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_ON, 0, note, 100)
    msg
  }

  def noteOff(note: Int): MidiMessage = {
    val msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_OFF, 0, note, 0)
    msg
  }

  def run: IO[Unit] =
    ReactiveSynth.resource[IO].use { case (queue, synthStream) =>

      val producer =
        Stream.eval(queue.offer(noteOn(60))) ++
        Stream.sleep_[IO](scala.concurrent.duration.DurationInt(500).millis) ++
        Stream.eval(queue.offer(noteOff(60)))

      synthStream
        .concurrently(producer)
        .compile
        .drain
    }
}