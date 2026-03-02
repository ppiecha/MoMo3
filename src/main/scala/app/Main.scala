import cats.effect._
import fs2.Stream
import javax.sound.midi._
import app.midi.ReactiveSynth

// to

object Main extends IOApp.Simple {
  // Utility to create PROGRAM_CHANGE message
  def programChange(channel: Int, program: Int): MidiMessage = {
    val msg = new ShortMessage()
    msg.setMessage(ShortMessage.PROGRAM_CHANGE, channel, program, 0)
    msg
  }

  def noteOn(note: Int, channel: Int): MidiMessage = {
    val msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_ON, channel, note, 100)
    msg
  }

  def noteOff(note: Int, channel: Int): MidiMessage = {
    val msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_OFF, channel, note, 0)
    msg
  }

  def run: IO[Unit] = {
    // 1. Set instrument (Piano = 0) for each channel before playing notes
    val producerC: Stream[IO, MidiMessage] =
      Stream.emit(programChange(0, 0)) ++
      Stream.emit(noteOn(60, 0)) ++
      Stream.sleep_[IO](scala.concurrent.duration.DurationInt(500).millis) ++
      Stream.emit(noteOff(60, 0))

    val producerE: Stream[IO, MidiMessage] =
      Stream.emit(programChange(1, 0)) ++
      Stream.sleep_[IO](scala.concurrent.duration.DurationInt(100).millis) ++
      Stream.emit(noteOn(64, 1)) ++
      Stream.sleep_[IO](scala.concurrent.duration.DurationInt(500).millis) ++
      Stream.emit(noteOff(64, 1))

    val producerG: Stream[IO, MidiMessage] =
      Stream.emit(programChange(2, 0)) ++
      Stream.sleep_[IO](scala.concurrent.duration.DurationInt(200).millis) ++
      Stream.emit(noteOn(67, 2)) ++
      Stream.sleep_[IO](scala.concurrent.duration.DurationInt(500).millis) ++
      Stream.emit(noteOff(67, 2))

    val midiInputs = List(producerC, producerE, producerG)

    ReactiveSynth.resource[IO](midiInputs).use { case (_, all) => all.compile.drain }
  }
}