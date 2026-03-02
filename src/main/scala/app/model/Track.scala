package app.model

import app.*
import app.midi.*
import app.model.Generator
import cats.data.Kleisli
import cats.syntax.all.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import app.midi.Event.*
import javax.sound.midi
import javax.sound.midi.{Sequence, ShortMessage, MidiEvent}
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import scala.concurrent.duration.FiniteDuration

case class Track(channel: Channel, timeGen: Generator[Tick], durGen: Generator[Duration], noteGen: Generator[Note])
    extends Playable {

  private val parsedTime: App[LazyList[IsValid[Tick]]] = Generator.parse(timeGen)
  private val accumulatedTime = parsedTime.map(_.scan(Tick.from(0L))((acc, tick) => (acc, tick).mapN(_ + _)))

  def toMidiEvents: App[LazyList[Event]] =
    Kleisli { env =>
      val events = for {
        start    <- accumulatedTime
        note     <- Generator.parse(noteGen)
        duration <- Generator.parse(durSeq)
      } yield {
        start
          .zip(note)
          .zip(duration)
          .flatMap { case ((s, n), d) =>
            (s, n, d)
              .mapN((start, note, duration) => makeMidiEvents(channel, start, note, duration)) match {
              case Validated.Valid(events) => events.map(_.validNec[ValidationError])
              case Validated.Invalid(nec)  => LazyList(Invalid(nec))
            }
          }
      }
      val res = events.run(env)
      res match
        case Left(domainError) => Left(domainError)
        case Right(events)     => Track.attempt(events)
    }

  override def sequence(duration: Option[FiniteDuration]): App[Sequence] =
    for {
      env <- Kleisli.ask[ErrorOr, Env]
      sequence <- Track.addTrackToSequence(
        this,
        new Sequence(Sequence.PPQ, env.ctx.ppq.value),
        duration
      )
    } yield sequence
}

object Track {

  def addEventToTrack(event: MidiEvent, track: midi.Track, console: Console): Unit =
    if !track.add(event) then throw new RuntimeException(s"Failed to add event: $event to track")

  def attempt(events: LazyList[IsValid[Event]]): ErrorOr[LazyList[Event]] =
    val (errors, validEvents) = events.partitionMap {
      case Valid(event) => Right(event)
      case Invalid(nec) => Left(ValidationError.InvalidEvent(s"${nec.toList.mkString(", ")}"))
    }
    if errors.nonEmpty then
      Left(ValidationError.InvalidEvents(errors.toList.map(_.asInstanceOf[ValidationError.InvalidEvent])))
    else Right(validEvents)

  def addTrackToSequence(
      track: Track,
      sequence: Sequence,
      duration: Option[FiniteDuration]
  ): App[Sequence] =
    for {
      env    <- Kleisli.ask[ErrorOr, Env]
      events <- limitEvents(track.toMidiEvents.run(env), duration)
    } yield {
      val midiTrack = sequence.createTrack()
      events.flatMap(makeMidiEvent).foreach(event => addEventToTrack(event, midiTrack, env.console))
      sequence
    }
}
