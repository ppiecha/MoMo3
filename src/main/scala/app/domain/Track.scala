package app.domain

import app.domain.Generator
import app.domain.Generator._
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated.*
import app.domain.ValidationError
import cats.data.{NonEmptyList, ValidatedNec}
import app.syntax.Conversions.*

case class Track(timeGen: Generator[Tick], durGen: Generator[Tick], noteGen: Generator[Note], velGen: Generator[Velocity])(
  using ch: Channel
) {
  
  val channel: Channel = ch
  
  def ++(other: Track): Track =
    Track(
      timeGen = this.timeGen ++ other.timeGen,
      durGen = this.durGen ++ other.durGen,
      noteGen = this.noteGen ++ other.noteGen,
      velGen = this.velGen ++ other.velGen
    )
  
  def next(other: Track): Track = this ++ other
  
  def muted: Track = {
    this.copy(velGen = Track.velocity(Velocity.Zero, timeGen.length))
  }
}

object Track {
  
  def track(
    timeGen: Generator[Tick],
    durGen: Generator[Tick],
    noteGen: Generator[Note],
    velGen: Generator[Velocity]
  )(using ch: Channel): Track = {
    Track(timeGen, durGen, noteGen, velGen)
  }

  def track(
    timeGen: Generator[Tick],
    durGen: Generator[Tick],
    noteGen: Generator[Note],
  )(using ch: Channel): Track = {
    track(timeGen, durGen, noteGen, velocity(Velocity.Default, timeGen.length))
  }

  def track(
    timeGen: Generator[Tick],
    noteGen: Generator[Note]
  )(using ch: Channel): Track = {
    track(timeGen, timeGen, noteGen)
  }
  
  def rest(t: Double)(using channel: Channel): Track = {
    Track(timeGen = time(t), durGen = duration(t), noteGen = note(Note.Zero), velGen = velocity(Velocity.Zero))
  }
  
  def time(t: Double): Generator[Tick] = {
    TimeGen(Seq(t))
  }
  
//  def time(t: Double, length: Int): Generator[Tick] = {
//    TimeGen(Seq(t).repeat(length))
//  }
  
  def time(t: Double*): Generator[Tick] = {
    TimeGen(t)
  }
  
  def duration(d: Double): Generator[Tick] = {
      DurationGen(Seq(d))
  }
  
//  def duration(d: Double, length: Int): Generator[Tick] = {
//      DurationGen(Seq(d).repeat(length))
//  }
  
  def duration(d: Double*): Generator[Tick] = {
      DurationGen(d)
  }
  
  def note(n: Int): Generator[Note] = {
    NoteGen(Seq(n))
  }
  
//  def note(n: Int, length: Int): Generator[Note] = {
//      NoteGen(Seq(n).repeat(length))
//  }
  
  def note(n: Int*): Generator[Note] = {
      NoteGen(n)
  }
  
  def velocity(v: Int): Generator[Velocity] = {
    VelocityGen(Seq(v))
  }
  
//  def velocity(v: Int, length: Int): Generator[Velocity] = {
//    VelocityGen(Seq(v).repeat(length))
//  }
  
  def velocity(v: Int*): Generator[Velocity] = {
    VelocityGen(v)
  }
  
}
