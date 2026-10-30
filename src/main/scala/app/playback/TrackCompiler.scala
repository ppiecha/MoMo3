package app.playback

import app.domain.MidiCommand.*
import app.domain.*
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import cats.data.{NonEmptyList, ValidatedNec}
import cats.data.NonEmptyList
import cats.syntax.all.*

object TrackCompiler {

  private def validateAlignedLengths(track: Track): ValidatedNec[DomainError, Track] = {
    val timeLength     = track.timing.length
    val durationLength = track.duration.map(_.length).getOrElse(track.timing.length)
    val noteLength     = track.notes.map(_.length).getOrElse(track.timing.length)
    val velocityLength = track.velocity.map(_.length).getOrElse(track.timing.length)

    if timeLength == durationLength && durationLength == noteLength then track.validNec
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

  private def accumulateTimes(
    track: Track
  )(using timingContext: TimingContext): NonEmptyList[ValidatedNec[DomainError, Tick]] = {
    NonEmptyList.fromListUnsafe(
      Generator
        .parseTicks(track.timing)
        .toList
        .scan(Tick.Zero.validNec[DomainError])((acc, tick) => (acc, tick).mapN(_ + _))
    )
  }

  def compile(track: Track)(using
    timingContext: TimingContext
  ): ValidatedNec[DomainError, NonEmptyList[AbstractMidiEvent]] =
    validateAlignedLengths(track).andThen { track =>
      val at = accumulateTimes(track)
      val notes = track.notes
        .map(Generator.parseNotes)
        .getOrElse(Generator.parseNotes(Generator.note(NoteArg.Single(0)).repeat(track.timing.length)))
      val duration = track.duration
        .map(Generator.parseTicks)
        .getOrElse(Generator.parseTicks(track.timing))
      val validatedVelocityDefault =
        Generator.parseVelocity(Generator.velocity(Velocity.Default).repeat(track.timing.length))
      val velocity = track.velocity
        .map(Generator.parseVelocity)
        .getOrElse(validatedVelocityDefault)

      val dummy = (Tick.from(0), NonEmptyList.of(Note.from(0)), Tick.from(0))

      val compiledEvents =
        at
          .zip(notes)
          .zip(duration)
          .zipAll(velocity, dummy, velocity.head)
          .map { case (((t, chord), d), v) =>
            oneGenEventToAbsoluteMidiEvents(t, chord, d, v, track.channel)
          }
          .sequence
          .map(_.flatten)

      val bank    = Bank.from(track.instrument.map(_.bank).getOrElse(Bank.Default))
      val program = Program.from(track.instrument.map(_.program).getOrElse(Program.Default))

      (bank, program, compiledEvents).mapN { (bank, program, events) =>
        NonEmptyList(AbsoluteMidiEvent(Tick.Zero, ProgramChange(track.channel, bank, program)), events.toList)
      }
    }

  def oneGenEventToAbsoluteMidiEvents(
    at: ValidatedNec[DomainError, Tick],
    chord: ValidatedNec[DomainError, NonEmptyList[Note]],
    duration: ValidatedNec[DomainError, Tick],
    velocity: ValidatedNec[DomainError, Velocity],
    channel: Channel
  ): ValidatedNec[DomainError, NonEmptyList[AbstractMidiEvent]] = {
    (at, chord, duration, velocity, Velocity.from(Velocity.Zero)).mapN {
      (at, chord, duration, velocity, velocityZero) =>
        val nextAt = at + duration
        chord.flatMap { note =>
          if note.value == 0 ||
            velocity.value == 0 ||
            duration.value == 0
          then NonEmptyList.one(AbstractMidiEvent.RestEvent) // Skip 0 (rest)
          else
            NonEmptyList.of(
              AbsoluteMidiEvent(at, NoteOn(channel, note, velocity)),
              AbsoluteMidiEvent(nextAt, NoteOff(channel, note, velocityZero))
            )
        }
    }
  }
}
