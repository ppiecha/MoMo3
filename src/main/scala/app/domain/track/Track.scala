package app.domain

import app.domain.Generator
import app.domain.Generator.{NoteGen, VelocityGen}
import cats.data.{Ior, IorNec, NonEmptyList, NonEmptyChain as NEC}

case class Track(
  timing: TimeGen,
  notes: Option[Generator[Note]],
  duration: Option[DurationGen],
  velocity: Option[Generator[Velocity]],
  instrument: Option[Instrument]
)(using
  ch: Channel
) {

  val channel: Channel = ch

  def ++(other: Track): Track =
    Track(
      timing = TimeGen(NonEmptyList(this.timing.values.head, this.timing.values.tail ++ other.timing.values.toList)),
      notes = (this.notes, other.notes) match {
        case (Some(n1), Some(n2)) => Some(n1 ++ n2)
        case (Some(n1), None)     => Some(n1)
        case (None, Some(n2))     => Some(n2)
        case (None, None)         => None
      },
      duration = (this.duration, other.duration) match {
        case (Some(d1), Some(d2)) => Some(DurationGen(NonEmptyList(d1.values.head, d1.values.tail ++ d2.values.toList)))
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
      instrument = (this.instrument, other.instrument) match {
        case (Some(i1), Some(i2)) => Some(i1)
        case (Some(i1), None)     => Some(i1)
        case (None, Some(i2))     => Some(i2)
        case (None, None)         => None
      }
    )

  def muted: Track = {
    this.copy(velocity = Some(Generator.velocity(Velocity.Zero).repeat(timing.length)))
  }

  def compareDuration(duration: Double): IorNec[DomainError, Track] = {
    if duration < 0 then Ior.Left(NEC.one(DomainError.NegativeDuration(duration)))
    else if timing.duration != duration then Ior.Left(NEC.one(DomainError.DurationMismatch(timing.duration, duration, None)))
    else Ior.Right(this)
  }

}

object Track {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def track(
    timing: TimeGen,
    notes: Option[Generator[Note]],
    duration: Option[DurationGen] = None,
    velocity: Option[Generator[Velocity]] = None,
    instrument: Option[Instrument] = None
  )(using ch: Channel): Track = {
    Track(timing, notes, duration, velocity, instrument)
  }

  def rest(value: Double)(using channel: Channel): Track = {
    Track(
      timing = time(value),
      notes = None,
      duration = None,
      velocity = None,
      instrument = None
    )
  }

  def time(head: Double, tail: Double*): TimeGen = {
    TimeGen(NonEmptyList(head, tail.toList))
  }

  def duration(head: Double, tail: Double*): DurationGen = {
    DurationGen(NonEmptyList(head, tail.toList))
  }

  def note(head: NoteArg, tail: NoteArg*): Option[NoteGen] = {
    Some(Generator.note(head, tail*))
  }

  def velocity(head: Int, tail: Int*): Option[VelocityGen] = {
    Some(Generator.velocity(head, tail*))
  }
}
