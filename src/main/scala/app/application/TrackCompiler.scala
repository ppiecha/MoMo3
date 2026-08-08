package app.application

import cats.syntax.all.*
import cats.data.Validated.{Invalid, Valid}
import app.config.*
import app.shared.*
import app.midi.*
import app.domain.*
import app.domain.Generator.VelocityGen
import app.domain.MidiCommand.*

object TrackCompiler {

  def accumulateTimes(track: Track, timingContext: TimingContext): LazyList[IsValid[Tick]] =
    Generator
      .parse(track.timeGen, timingContext.ppq)
      .scan(Tick.zero.validNec[ValidationError])((acc, tick) => (acc, tick).mapN(_ + _))

  def eventList(track: Track, timingContext: TimingContext): LazyList[IsValid[AbsoluteMidiEvent]] = {
    val at = accumulateTimes(track, timingContext)
    val note = Generator.parse(track.noteGen, timingContext.ppq)
    val duration = Generator.parse(track.durGen, timingContext.ppq)
    val velocity = Generator.parse(track.velGen.getOrElse(VelocityGen(Velocity.infinityFromDefault)), timingContext.ppq)

    at
      .zip(note)
      .zip(duration)
      .zip(velocity)
      .flatMap { case (((t, n), d), v) =>
        val events: IsValid[(AbsoluteMidiEvent, AbsoluteMidiEvent)] =
          (track.channel, t, n, d, v).mapN { (ch, at, note, duration, velocity) =>
            val nextAt = at + duration
            (
              AbsoluteMidiEvent(at, NoteOn(ch, note, velocity)),
              AbsoluteMidiEvent(nextAt, NoteOff(ch, note))
            )
          }

        events.fold(
          errors => LazyList(errors.invalid),
          { case (on, off) => LazyList(on.validNec, off.validNec) }
        )
      }
  }

  def compile(track: Track, timingContext: TimingContext): CompiledTrack = {
    CompiledTrack(eventList(track, timingContext).map {
      case Valid(event)    => Right(event)
      case Invalid(errors) => Left(ValidationError.InvalidEvent(errors.toList))
    })
  }

}
