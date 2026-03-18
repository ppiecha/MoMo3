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


case class TrackOutput[F[_]: Async](midiStream: MidiStream[F], midiEventsStream: Stream[Pure, MidiEvent])

case class Track(
    channel: Channel,
    timeGen: Generator[Time],
    durGen: Generator[Time],
    noteGen: Generator[Note]
) extends Playable {

  private def parsedTime[F[_]: Async]: App[F, Stream[Pure, IsValid[Time]]] = {
    val app = Generator.parse(timeGen)
    app.flatMap { stream =>
      liftFPure(
        stream.scan(Time.zero.validNec[ValidationError])((acc, time) => (acc, time).mapN((a, t) => Time(t.duration, a.tick + t.tick)))
      )
    }
  }
  // private def accumulatedTime[F[_]: Async]: App[F, Stream[Pure, IsValid[FiniteDuration]]] =
  //   parsedTime[F].map(_.scan(0.millis.validNec[ValidationError])((acc, tick) => (acc, tick).mapN(_ + _)))
  // private def startTime[F[_]: Async]: App[F, Stream[Pure, IsValid[FiniteDuration]]] = 
  //   parsedTime[F].map(s => Stream(0.millis.validNec[ValidationError]) ++ s)

  def eventStream[F[_]: Async]: App[F, EventStream[F]] =
    for {
      time    <- parsedTime
      note     <- Generator.parse(noteGen)
      duration <- Generator.parse(durGen)
    } yield {
      time
        .zip(note)
        .zip(duration)
        .flatMap { case ((t, n), d) =>
          (t, n, d)
            .mapN((time, note, duration) => Event.makeStream(channel, time, note, duration)) match {
            case Valid(events) => events
            case Invalid(nec) =>
              Stream.raiseError[F](new RuntimeException("Invalid MIDI message: " + nec.toList.mkString(", ")))
          }
        }
    }

  def eventStreamToOutput[F[_]](eventStream: EventStream[F]): TrackOutput[F] = ???
  
  def output[F[_]: Async]: App[F, TrackOutput[F]] = eventStream.map(eventStreamToOutput)

  def midiStream[F[_]: Async]: App[F, MidiStream[F]] = ???
  override def midiStreams[F[_]: Async]: App[F, List[MidiStream[F]]] = midiStream[F].map(List(_))

}

object Track {
  def fromFile[F[_]: Async](filePath: String): App[F, Track] =
    ???
}