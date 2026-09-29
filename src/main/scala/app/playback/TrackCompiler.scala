package app.playback

import app.domain.*
import app.domain.MidiCommand.*
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.syntax.all.*

object TrackCompiler {

  private def accumulateTimes(track: Track, timingContext: TimingContext): Seq[ValidatedNec[ValidationError, Tick]] =
    Generator
      .parseTicks(track.timeGen, timingContext.ppq)
      .scan(Tick.zero.validNec[ValidationError])((acc, tick) => (acc, tick).mapN(_ + _))

  def compile(
    track: Track,
    timingContext: TimingContext
  ): ValidatedNec[ValidationError, Seq[AbsoluteMidiEvent]] = {
    val at       = accumulateTimes(track, timingContext)
    val note     = Generator.parse(track.noteGen, timingContext.ppq)
    val duration = Generator.parseTicks(track.durGen, timingContext.ppq)
    val velocity = Generator.parse(track.velGen, timingContext.ppq)

    at
      .zip(note)
      .zip(duration)
      .zip(velocity)
      .flatMap { case (((t, n), d), v) =>
        val events: ValidatedNec[ValidationError, (AbsoluteMidiEvent, AbsoluteMidiEvent)] =
          (t, n, d, v).mapN { (at, note, duration, velocity) =>
            val nextAt = at + duration
            (
              AbsoluteMidiEvent(at, NoteOn(track.channel, note, velocity)),
              AbsoluteMidiEvent(nextAt, NoteOff(track.channel, note))
            )
          }
        events.fold(
          errors => Seq(errors.invalid[AbsoluteMidiEvent]),
          { case (on, off) => Seq(on.validNec, off.validNec) }
        )
      }
      .sequence
  }

}
