package app.playback

import app.domain.MidiCommand._
import app.domain._
import cats.data.ValidatedNec
import cats.data.{NonEmptyList => NEL}
import cats.syntax.all._

object TrackCompiler {

  private def validateAlignedLengths(track: Track): ValidatedNec[DomainError, Track] = {
    val timeLength     = track.timing.length
    val durationLength = track.duration.map(_.length).getOrElse(track.timing.length)
    val noteLength     = track.notes.length
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

  private def accumulateTimes(track: Track)(using timingContext: TimingContext): Seq[ValidatedNec[DomainError, Tick]] =
    Generator
      .parseTicks(track.timing)
      .scan(Tick.Zero.validNec[DomainError])((acc, tick) => (acc, tick).mapN(_ + _))

  def compile(track: Track)(using timingContext: TimingContext): ValidatedNec[DomainError, Seq[AbsoluteMidiEvent]] =
    validateAlignedLengths(track).andThen { track =>
      val at    = accumulateTimes(track)
      val notes = Generator.parseNotes(track.notes)
      val duration = track.duration
        .map(Generator.parseTicks)
        .getOrElse(Generator.parseTicks(track.timing))
      val velocity = track.velocity
        .map(Generator.parseVelocity)
        .getOrElse(Generator.parseVelocity(Generator.velocity(Velocity.Default).repeat(track.timing.length)))

      val dummy = (Tick.from(0), NEL.of(Note.from(0)), Tick.from(0))

      val compiledEvents = at
        .zip(notes)
        .zip(duration)
        .zipAll(velocity, dummy, velocity.head)
        .map { case (((t, chord), d), v) =>
          oneGenEventToAbsoluteMidiEvents(t, chord, d, v, track.channel)
        }
        .sequence
        .map(_.flatten)

      val bank    = Bank.from(track.instrument.bank)
      val program = Program.from(track.instrument.program)

      (bank, program, compiledEvents).mapN { (bank, program, events) =>
        AbsoluteMidiEvent(Tick.Zero, ProgramChange(track.channel, bank, program)) +: events
      }
    }

  def oneGenEventToAbsoluteMidiEvents(
    at: ValidatedNec[DomainError, Tick],
    chord: ValidatedNec[DomainError, NEL[Note]],
    duration: ValidatedNec[DomainError, Tick],
    velocity: ValidatedNec[DomainError, Velocity],
    channel: Channel
  ): ValidatedNec[DomainError, Seq[AbsoluteMidiEvent]] = {
    (at, chord, duration, velocity).mapN { (at, chord, duration, velocity) =>
      val nextAt = at + duration
      chord.toList.flatMap { note =>
        if note.value == 0 ||
           velocity.value == 0 ||
           duration.value == 0
        then Seq.empty // Skip 0 (rest)
        else
          Seq(
            AbsoluteMidiEvent(at, NoteOn(channel, note, velocity)),
            AbsoluteMidiEvent(nextAt, NoteOff(channel, note))
          )
//        note.value match {
//          case 0 => Seq.empty // Skip note 0 (rest)
//          case _ =>
//            Seq(
//              AbsoluteMidiEvent(at, NoteOn(channel, note, velocity)),
//              AbsoluteMidiEvent(nextAt, NoteOff(channel, note))
//            )
//        }
      }
    }
  }
}
