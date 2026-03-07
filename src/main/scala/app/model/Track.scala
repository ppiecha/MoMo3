package app.model

import app.*
import app.midi.*
import app.midi.Message.*
import app.model.Generator
import cats.syntax.all.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import javax.sound.midi
import javax.sound.midi.{Sequence, ShortMessage, MidiEvent}
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import scala.concurrent.duration.*
import cats.effect.kernel.*
import fs2.*

case class Track(
    channel: Channel,
    timeGen: Generator[FiniteDuration],
    durGen: Generator[FiniteDuration],
    noteGen: Generator[Note]
) extends Playable {

  private def parsedTime[F[_]: Async]: App[F, Stream[Pure, IsValid[FiniteDuration]]] = Generator.parse(timeGen)
  private def accumulatedTime[F[_]: Async]: App[F, Stream[Pure, IsValid[FiniteDuration]]] =
    parsedTime[F].map(_.scan(0.millis.validNec[ValidationError])((acc, tick) => (acc, tick).mapN(_ + _)))

  def midiStream[F[_]: Async]: App[F, MidiStream[F]] =
    for {
      start    <- accumulatedTime
      note     <- Generator.parse(noteGen)
      duration <- Generator.parse(durGen)
    } yield {
      start
        .zip(note)
        .zip(duration)
        .flatMap { case ((s, n), d) =>
          (s, n, d)
            .mapN((start, note, duration) => makeMidiStream(channel, start, note, duration)) match {
            case Valid(events) => events
            case Invalid(nec) =>
              Stream.raiseError[F](new RuntimeException("Invalid MIDI message: " + nec.toList.mkString(", ")))
          }
        }
    }

  override def midiStreams[F[_]: Async]: App[F, List[MidiStream[F]]] = midiStream[F].map(List(_))

}