package app.application

import cats.syntax.all.*
import cats.data.Validated.{Invalid, Valid}

import app.config.*
import app.shared.*
import app.midi.*
import app.domain.*
import app.domain.MidiCommand.*

object TrackCompiler {

  def accumulateTimes(track: Track, env: Environment): LazyList[IsValid[Tick]] =
    Generator
      .parse(track.timeGen, env)
      .scan(Tick.zero.validNec[ValidationError])((acc, tick) => (acc, tick).mapN(_ + _))

  def eventList(track: Track, env: Environment): LazyList[IsValid[AbsoluteMidiEvent]] = {
    val at       = accumulateTimes(track, env)
    val note     = Generator.parse(track.noteGen, env)
    val duration = Generator.parse(track.durGen, env)

    at
      .zip(note)
      .zip(duration)
      .flatMap { case ((t, n), d) =>
        val events: IsValid[(AbsoluteMidiEvent, AbsoluteMidiEvent)] =
          (t, n, d).mapN { (at, note, duration) =>
            val nextAt = at + duration
            (
              AbsoluteMidiEvent(at, NoteOn(track.channel, note, MidiValue.unsafe(100))),
              AbsoluteMidiEvent(nextAt, NoteOff(track.channel, note))
            )
          }

        events.fold(
          errors => LazyList(errors.invalid),
          { case (on, off) => LazyList(on.validNec, off.validNec) }
        )
      }
  }

  def compile(track: Track, env: Environment): CompiledTrack = {
    CompiledTrack(eventList(track, env).map {
      case Valid(event)    => Right(event)
      case Invalid(errors) => Left(ValidationError.InvalidEvent(errors.toList))
    })
  }

}
