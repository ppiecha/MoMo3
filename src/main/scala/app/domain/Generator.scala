package app.domain

import app.syntax.Extensions.repeat
import cats.data.ValidatedNec

sealed trait TickGenerator { def values: Seq[Double] }

final case class TimeGen(values: Seq[Double]) extends TickGenerator {
  def repeat(n: Int): TimeGen     = TimeGen(values.repeat(n))
  def length: Int                 = values.length
  def ++(other: TimeGen): TimeGen = TimeGen(values ++ other.values)
  def duration: Double            = if length == 0 then 0 else 1 / values.map(v => 1 / v).sum
}

final case class DurationGen(values: Seq[Double]) extends TickGenerator {
  def repeat(n: Int): DurationGen         = DurationGen(values.repeat(n))
  def length: Int                         = values.length
  def ++(other: DurationGen): DurationGen = DurationGen(values ++ other.values)
}

enum Generator[A]:
  case NoteGen(s: Seq[Int])     extends Generator[Note]
  case VelocityGen(s: Seq[Int]) extends Generator[Velocity]

  def ++(other: Generator[A]): Generator[A] = (this, other) match
    case (NoteGen(s1), NoteGen(s2))         => NoteGen(s1 ++ s2)
    case (VelocityGen(s1), VelocityGen(s2)) => VelocityGen(s1 ++ s2)

  def length: Int = this match
    case NoteGen(s)     => s.length
    case VelocityGen(s) => s.length

  def repeat(n: Int): Generator[A] = this match
    case NoteGen(s)     => NoteGen(s.repeat(n))
    case VelocityGen(s) => VelocityGen(s.repeat(n))

object Generator {

  def parse[A](seq: Generator[A], ppq: Ppq): Seq[ValidatedNec[ValidationError, A]] =
    seq match {
      case NoteGen(s)     => s.map(MidiValue[NoteTag])
      case VelocityGen(s) => s.map(MidiValue[VelocityTag])
    }

  def parseTicks(seq: TickGenerator, ppq: Ppq): Seq[ValidatedNec[ValidationError, Tick]] =
    seq.values.map(d => Tick.fromDouble(d, ppq))

}
