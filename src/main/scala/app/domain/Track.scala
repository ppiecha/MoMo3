package app.domain

import app.domain.Generator
import app.domain.Generator._
import cats.data.Ior
import cats.data.IorNec
import cats.data.{NonEmptyChain => NEC}

case class Track(
  timeGen: TimeGen,
  durGen: DurationGen,
  noteGen: Generator[Note],
  velGen: Generator[Velocity]
)(using
  ch: Channel
) {

  val channel: Channel = ch

  def ++(other: Track): Track =
    Track(
      timeGen = TimeGen(this.timeGen.values ++ other.timeGen.values),
      durGen = DurationGen(this.durGen.values ++ other.durGen.values),
      noteGen = this.noteGen ++ other.noteGen,
      velGen = this.velGen ++ other.velGen
    )

  def muted: Track = {
    this.copy(velGen = Track.velocity(Velocity.Zero).repeat(timeGen.length))
  }

  def compareDuration(duration: Double): IorNec[DomainError, Track] = {
    if duration < 0 then Ior.Left(NEC.one(DomainError.NegativeDuration(duration)))
    else if timeGen.duration != duration then
      Ior.Left(NEC.one(DomainError.DurationMismatch(timeGen.duration, duration)))
    else Ior.Right(this)
  }

}

object Track {

  def empty(using ch: Channel): Track = {
    Track(
      timeGen = TimeGen(Seq.empty),
      durGen = DurationGen(Seq.empty),
      noteGen = Generator.NoteGen(Seq.empty),
      velGen = Generator.VelocityGen(Seq.empty)
    )
  }

  def track(
    timeGen: TimeGen,
    durGen: DurationGen,
    noteGen: Generator[Note],
    velGen: Generator[Velocity]
  )(using ch: Channel): Track = {
    Track(timeGen, durGen, noteGen, velGen)
  }

  def track(
    timeGen: TimeGen,
    durGen: DurationGen,
    noteGen: Generator[Note]
  )(using ch: Channel): Track = {
    track(timeGen, durGen, noteGen, velocity(Velocity.Default).repeat(timeGen.length))
  }

  def track(
    timeGen: TimeGen,
    noteGen: Generator[Note]
  )(using ch: Channel): Track = {
    track(timeGen, DurationGen(timeGen.values), noteGen)
  }

  def rest(duration: Double)(using channel: Channel): Track = {
    Track(
      timeGen = time(duration),
      durGen = Track.duration(duration),
      noteGen = note(Note.Zero),
      velGen = velocity(Velocity.Zero)
    )
  }
}
