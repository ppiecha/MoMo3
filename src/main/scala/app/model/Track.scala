package app.model

import app.*
import app.midi.*
import app.midi.Message.*
import app.model.Generator
import cats.syntax.all.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import scala.concurrent.duration.*
import cats.effect.kernel.*
import fs2.*
import javax.sound.midi.*

case class Track(
    channel: Channel,
    timeGen: Generator[Time],
    durGen: Generator[Time],
    noteGen: Generator[Note]
) extends Playable {

  def parsedTime[F[_]: Async]: App[F, LazyList[IsValid[Time]]] = {
    val app = Generator.parse(timeGen)
    app.flatMap { ll =>
      liftFPure(
        ll.scan(Time.zero.validNec[ValidationError])((acc, time) =>
          (acc, time).mapN((a, t) => Time(t.duration, a.tick + t.tick))
        ).sliding(2).map(l2 => l2.toList match {
          case List(t1, t2) => (t1, t2).mapN((curr, next) => Time(next.duration, curr.tick))
          case List(t2) => t2                 
          case _ => throw new RuntimeException("Empty list in sliding window, this should never happen")
        }).to(LazyList))
    }
  }
  // private def accumulatedTime[F[_]: Async]: App[F, Stream[Pure, IsValid[FiniteDuration]]] =
  //   parsedTime[F].map(_.scan(0.millis.validNec[ValidationError])((acc, tick) => (acc, tick).mapN(_ + _)))
  // private def startTime[F[_]: Async]: App[F, Stream[Pure, IsValid[FiniteDuration]]] =
  //   parsedTime[F].map(s => Stream(0.millis.validNec[ValidationError]) ++ s)

  def eventList[F[_]: Async]: App[F, LazyList[Event]] =
    for {
      time     <- parsedTime
      note     <- Generator.parse(noteGen)
      duration <- Generator.parse(durGen)
    } yield {
      time
        .zip(note)
        .zip(duration)
        .flatMap { case ((t, n), d) =>
          (t, n, d)
            .mapN((time, note, duration) => Event.makeList(channel, time, note, duration)) match {
            case Valid(events) => events
            case Invalid(nec)  => throw new RuntimeException("Invalid MIDI message: " + nec.toList.mkString(", "))
          }
        }
    }

  def eventListToString[F[_]: Async](number: Int): App[F, String] = eventList.map { events =>
    s"List(${events.take(number).map(_.toString).mkString("\n  ", ",\n  ", "\n")}${if (events.size > number) ", ..." else ""})"
  }

  def eventListToOutput[F[_]: Async](eventList: LazyList[Event]): TrackOutput[F] = {
    val midiStream: Stream[F, Stream[F, ShortMessage]] =
      Stream.emits(eventList)
        .flatMap(event => Stream(event.streamOfMidiMessages[F]) ++ Stream.sleep_[F](event.time.duration))
    val midiEventList: LazyList[MidiEvent] = eventList.flatMap(_.listOfMidiEvents)
    TrackOutput(midiStream, midiEventList)
  }

  def output[F[_]: Async]: App[F, TrackOutput[F]] = eventList.map(eventListToOutput)

  def midiStream[F[_]: Async]: App[F, Stream[F, Stream[F, ShortMessage]]]                 = output.map(_.midiStream)
  override def midiStreams[F[_]: Async]: App[F, List[Stream[F, Stream[F, ShortMessage]]]] = midiStream[F].map(List(_))

}

case class TrackOutput[F[_]: Async](
    midiStream: Stream[F, Stream[F, ShortMessage]],
    midiEventList: LazyList[MidiEvent]
) {

  def toMidiFile = ???
}

object Track {
  def fromFile[F[_]: Async](filePath: String): App[F, Track] =
    ???
}
