package app.domain

import app.domain.Generator
import cats.data.Ior
import cats.data.IorNec
import cats.data.{NonEmptyChain => NEC}

case class Track(
  timing: TimeGen,
  notes: Generator[Note],
  duration: Option[DurationGen],
  velocity: Option[Generator[Velocity]],
  instrument: Instrument = Instrument.Default
)(using
  ch: Channel
) {

  val channel: Channel = ch

  def initialControls: Seq[MidiCommand] = ???

  def ++(other: Track): Track =
    Track(
      timing = TimeGen(this.timing.values ++ other.timing.values),
      notes = this.notes ++ other.notes,
      duration = (this.duration, other.duration) match {
        case (Some(d1), Some(d2)) => Some(DurationGen(d1.values ++ d2.values))
        case (Some(d1), None)     => Some(d1)
        case (None, Some(d2))     => Some(d2)
        case (None, None)         => None
      },
      velocity = (this.velocity, other.velocity) match {
        case (Some(v1), Some(v2)) => Some(v1 ++ v2)
        case (Some(v1), None)     => Some(v1)
        case (None, Some(v2))     => Some(v2)
        case (None, None)         => None
      },
      instrument = this.instrument
    )

  def muted: Track = {
    this.copy(velocity = Some(Generator.velocity(Velocity.Zero).repeat(timing.length)))
  }

  def compareDuration(duration: Double): IorNec[DomainError, Track] = {
    if duration < 0 then Ior.Left(NEC.one(DomainError.NegativeDuration(duration)))
    else if timing.duration != duration then Ior.Left(NEC.one(DomainError.DurationMismatch(timing.duration, duration)))
    else Ior.Right(this)
  }

}

object Track {

  def track(
    timing: TimeGen,
    notes: Generator[Note],
    duration: Option[DurationGen] = None,
    velocity: Option[Generator[Velocity]] = None,
    instrument: Instrument = Instrument.Default
  )(using ch: Channel): Track = {
    Track(timing, notes, duration, velocity, instrument)
  }

  def empty(using ch: Channel): Track = {
    Track(
      timing = TimeGen(Seq.empty),
      notes = Generator.NoteGen(Seq.empty),
      duration = Some(DurationGen(Seq.empty)),
      velocity = Some(Generator.VelocityGen(Seq.empty))
    )
  }

  def rest(value: Double)(using channel: Channel): Track = {
    Track(
      timing = time(value),
      notes = note(Note.Zero),
      duration = Some(duration(value)),
      velocity = Some(velocity(Velocity.Zero))
    )
  }

  def time(t: Double*): TimeGen = {
    TimeGen(t)
  }

  def duration(d: Double*): DurationGen = {
    DurationGen(d)
  }

  def note(args: NoteArg*): Generator[Note] = {
    Generator.note(args*)
  }

  def velocity(v: Int*): Generator[Velocity] = {
    Generator.velocity(v*)
  }
}
