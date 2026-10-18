package app.domain

import app.domain.Generator
import app.domain.Generator._
import cats.data.Ior
import cats.data.IorNec
import cats.data.{NonEmptyChain => NEC}

case class Track(
  timeGen: TimeGen,
  noteGen: Generator[Note],
  durGen: Option[DurationGen],
  velGen: Option[Generator[Velocity]],
  instrument: Instrument = Instrument.Default
)(using
  ch: Channel
) {

  val channel: Channel = ch

  def ++(other: Track): Track =
    Track(
      timeGen = TimeGen(this.timeGen.values ++ other.timeGen.values),
      noteGen = this.noteGen ++ other.noteGen,
      durGen = (this.durGen, other.durGen) match {
        case (Some(d1), Some(d2)) => Some(DurationGen(d1.values ++ d2.values))
        case (Some(d1), None)     => Some(d1)
        case (None, Some(d2))     => Some(d2)
        case (None, None)         => None
      },
      velGen = (this.velGen, other.velGen) match {
        case (Some(v1), Some(v2)) => Some(v1 ++ v2)
        case (Some(v1), None)     => Some(v1)
        case (None, Some(v2))     => Some(v2)
        case (None, None)         => None
      }
    )

  def muted: Track = {
    this.copy(velGen = Some(velocity(Velocity.Zero).repeat(timeGen.length)))
  }

  def compareDuration(duration: Double): IorNec[DomainError, Track] = {
    if duration < 0 then Ior.Left(NEC.one(DomainError.NegativeDuration(duration)))
    else if timeGen.duration != duration then
      Ior.Left(NEC.one(DomainError.DurationMismatch(timeGen.duration, duration)))
    else Ior.Right(this)
  }

}

object Track {

  def track(
    timeGen: TimeGen,
    noteGen: Generator[Note],
    durGen: Option[DurationGen] = None,
    velGen: Option[Generator[Velocity]] = None,
    instrument: Instrument = Instrument.Default
  )(using ch: Channel): Track = {
    Track(timeGen, noteGen, durGen, velGen)
  }

  def empty(using ch: Channel): Track = {
    Track(
      timeGen = TimeGen(Seq.empty),
      noteGen = Generator.NoteGen(Seq.empty),
      durGen = Some(DurationGen(Seq.empty)),
      velGen = Some(Generator.VelocityGen(Seq.empty))
    )
  }

  def rest(value: Double)(using channel: Channel): Track = {
    Track(
      timeGen = time(value),
      noteGen = note(Note.Zero),
      durGen = Some(duration(value)),
      velGen = Some(velocity(Velocity.Zero))
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
