package app.playback

import app.domain.MidiCommand._
import app.domain.MidiValue.given
import app.domain._
import cats.data.NonEmptyList
import cats.data.ValidatedNec
import cats.syntax.all._

object TrackCompiler {

  private def validateAlignedLengths(track: Track): ValidatedNec[DomainError, Track] = {
    val timeLength     = track.timeGen.length
    val durationLength = track.durGen.map(_.length).getOrElse(track.timeGen.length)
    val noteLength     = track.noteGen.length
    val velocityLength = track.velGen.map(_.length).getOrElse(track.timeGen.length)

    if timeLength == durationLength && durationLength == noteLength && noteLength == velocityLength then track.validNec
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
      .scan(Tick.Zero.validNec[DomainError])((acc, tick) => (acc, tick).mapN(_ + _))

  def compile(
    track: Track,
    timingContext: TimingContext
  ): ValidatedNec[DomainError, Seq[AbsoluteMidiEvent]] =
    validateAlignedLengths(track).andThen { track =>
      val at    = accumulateTimes(track, timingContext)
      val notes = Generator.parseNotes(track.noteGen)
      val duration = track.durGen
        .map(Generator.parseTicks(_, timingContext.ppq))
        .getOrElse(Generator.parseTicks(track.timeGen, timingContext.ppq))
      val velocity = track.velGen
        .map(Generator.parseVelocity)
        .getOrElse(Generator.parseVelocity(Generator.velocity(Velocity.Default).repeat(track.timeGen.length)))

      at
        .zip(notes)
        .zip(duration)
        .zip(velocity)
        .flatMap { case (((t, chord), d), v) =>
          val events: ValidatedNec[DomainError, Seq[AbsoluteMidiEvent]] =
            oneGenEventToAbsoluteMidiEvents(t, chord, d, v, track.channel)
          val bank    = MidiValue[BankTag](track.instrument.bank)
          val program = MidiValue[ProgramTag](track.instrument.program)
          val eventsWithInstrument = (bank, program, events).mapN { (bank, program, events) =>
            AbsoluteMidiEvent(Tick.Zero, ProgramChange(track.channel, bank, program)) +: events
          }
          eventsWithInstrument.fold(
            errors => Seq(errors.invalid[AbsoluteMidiEvent]),
            chordEvents => chordEvents.map(_.validNec)
          )
        }
        .sequence
    }

  def oneGenEventToAbsoluteMidiEvents(
    at: ValidatedNec[DomainError, Tick],
    chord: ValidatedNec[DomainError, NonEmptyList[Note]],
    duration: ValidatedNec[DomainError, Tick],
    velocity: ValidatedNec[DomainError, Velocity],
    channel: Channel
  ): ValidatedNec[DomainError, Seq[AbsoluteMidiEvent]] = {
    (at, chord, duration, velocity).mapN { (at, chord, duration, velocity) =>
      val nextAt = at + duration
      chord.toList.flatMap { note =>
        note.value match {
          case 0 => Seq.empty // Skip note 0 (rest)
          case _ =>
            Seq(
              AbsoluteMidiEvent(at, NoteOn(channel, note, velocity)),
              AbsoluteMidiEvent(nextAt, NoteOff(channel, note))
            )
        }
      }
    }
  }
}
