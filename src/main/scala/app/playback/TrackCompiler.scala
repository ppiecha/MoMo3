package app.playback

import app.domain.MidiCommand._
import app.domain._
import cats.data.ValidatedNec
import cats.syntax.all._

object TrackCompiler {

  private def validateAlignedLengths(track: Track): ValidatedNec[DomainError, Unit] = {
    val timeLength     = track.timeGen.length
    val durationLength = track.durGen.length
    val noteLength     = track.noteGen.length
    val velocityLength = track.velGen.length

    if timeLength == durationLength && durationLength == noteLength && noteLength == velocityLength then ().validNec
    else
      DomainError
        .TrackLengthMismatch(
          time = timeLength,
          duration = durationLength,
          notes = noteLength,
          velocity = velocityLength
        )
        .invalidNec
  }

  private def accumulateTimes(track: Track, timingContext: TimingContext): Seq[ValidatedNec[DomainError, Tick]] =
    Generator
      .parseTicks(track.timeGen, timingContext.ppq)
      .scan(Tick.zero.validNec[DomainError])((acc, tick) => (acc, tick).mapN(_ + _))

  def compile(
    track: Track,
    timingContext: TimingContext
  ): ValidatedNec[DomainError, Seq[AbsoluteMidiEvent]] =
    validateAlignedLengths(track).andThen { _ =>
      val at       = accumulateTimes(track, timingContext)
      val notes    = Generator.parseNotes(track.noteGen)
      val duration = Generator.parseTicks(track.durGen, timingContext.ppq)
      val velocity = Generator.parseVelocity(track.velGen)

      at
        .zip(notes)
        .zip(duration)
        .zip(velocity)
        .flatMap { case (((t, chord), d), v) =>
          val events: ValidatedNec[DomainError, Seq[AbsoluteMidiEvent]] =
            (t, chord, d, v).mapN { (at, chord, duration, velocity) =>
              val nextAt = at + duration
              chord.toList.flatMap { note =>
                Seq(
                  AbsoluteMidiEvent(at, NoteOn(track.channel, note, velocity)),
                  AbsoluteMidiEvent(nextAt, NoteOff(track.channel, note))
                )
              }
            }
          events.fold(
            errors => Seq(errors.invalid[AbsoluteMidiEvent]),
            chordEvents => chordEvents.map(_.validNec)
          )
        }
        .sequence
    }

}
