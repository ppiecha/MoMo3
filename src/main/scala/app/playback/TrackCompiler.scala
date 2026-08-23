package app.playback

import app.domain.*
import app.domain.Generator.VelocityGen
import app.domain.MidiCommand.*
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.syntax.all.*

object TrackCompiler {

  def accumulateTimes(track: Track, timingContext: TimingContext): Seq[ValidatedNec[ValidationError, Tick]] =
    Generator
      .parse(track.timeGen, timingContext.ppq)
      .scan(Tick.zero.validNec[ValidationError])((acc, tick) => (acc, tick).mapN(_ + _))

  def eventList(
    track: Track,
    timingContext: TimingContext
  ): Seq[ValidatedNec[ValidationError, AbsoluteMidiEvent]] = {
    val at       = accumulateTimes(track, timingContext)
    val note     = Generator.parse(track.noteGen, timingContext.ppq)
    val duration = Generator.parse(track.durGen, timingContext.ppq)
    val velocity = Generator.parse(
      track.velGen.getOrElse(VelocityGen(Seq.fill(note.length)(VelocityTag.DEFAULT_VALUE))),
      timingContext.ppq
    )

    at
      .zip(note)
      .zip(duration)
      .zip(velocity)
      .flatMap { case (((t, n), d), v) =>
        val events: ValidatedNec[ValidationError, (AbsoluteMidiEvent, AbsoluteMidiEvent)] =
          (track.channel, t, n, d, v).mapN { (ch, at, note, duration, velocity) =>
            val nextAt = at + duration
            (
              AbsoluteMidiEvent(at, NoteOn(ch, note, velocity)),
              AbsoluteMidiEvent(nextAt, NoteOff(ch, note))
            )
          }

        events.fold(
          errors => Seq(errors.invalid),
          { case (on, off) => Seq(on.validNec, off.validNec) }
        )
      }
  }

  def compile(track: Track, timingContext: TimingContext): CompiledTrack = {
    CompiledTrack(eventList(track, timingContext).map {
      case Valid(event)    => Right(event)
      case Invalid(errors) => Left(DomainError.TrackCompilationFailed(ValidationError.InvalidEvent(errors.toList)))
    })
  }

}
