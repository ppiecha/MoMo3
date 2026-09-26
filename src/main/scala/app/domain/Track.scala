package app.domain

import app.domain.Generator
import app.domain.Generator._
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated.*
import app.domain.ValidationError
import cats.data.{NonEmptyList, ValidatedNec}
import app.syntax.Conversions.*

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
  
  def validatedDuration(duration: Double): ValidatedNec[ValidationError, Track] = {
    if duration < 0 then 
      ValidationError.NegativeDuration(duration).invalidNec
    if timeGen.duration != duration then 
      ValidationError.DurationMismatch(timeGen.duration, duration).invalidNec
    else 
      this.validNec
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

  def time(t: Double*): TimeGen = {
    TimeGen(t)
  }

  def duration(d: Double*): DurationGen = {
    DurationGen(d)
  }

  def note(n: Int*): Generator[Note] = {
    NoteGen(n)
  }

  def velocity(v: Int*): Generator[Velocity] = {
    VelocityGen(v)
  }

}
