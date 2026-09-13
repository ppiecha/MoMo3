package app.domain

import app.syntax.Conversions.repeat
import cats.data.ValidatedNec

enum Generator[A] {
  case TimeGen(s: Seq[Double])     extends Generator[Tick]
  case DurationGen(s: Seq[Double]) extends Generator[Tick]
  case NoteGen(s: Seq[Int])        extends Generator[Note]
  case VelocityGen(s: Seq[Int])    extends Generator[Velocity]
  
  def ++(other: Generator[A]): Generator[A] = (this, other) match {
    case (TimeGen(s1), TimeGen(s2))         => TimeGen(s1 ++ s2)
    case (NoteGen(s1), NoteGen(s2))         => NoteGen(s1 ++ s2)
    case (DurationGen(s1), DurationGen(s2)) => DurationGen(s1 ++ s2)
    case (VelocityGen(s1), VelocityGen(s2)) => VelocityGen(s1 ++ s2)
    case _                                  => throw new IllegalArgumentException("Cannot combine different types of generators")
  }
  
  def length: Int = this match {
    case TimeGen(s)     => s.length
    case NoteGen(s)     => s.length
    case DurationGen(s) => s.length
    case VelocityGen(s) => s.length
  }
  
  def repeat(n: Int): Generator[A] = this match {
    case TimeGen(s)     => TimeGen(s.repeat(n))
    case NoteGen(s)     => NoteGen(s.repeat(n))
    case DurationGen(s) => DurationGen(s.repeat(n))
    case VelocityGen(s) => VelocityGen(s.repeat(n))
  }
}

object Generator {

  def parse[A](seq: Generator[A], ppq: Ppq): Seq[ValidatedNec[ValidationError, A]] =
    seq match {
      case TimeGen(s)     => s.map(d => Tick.fromDouble(d, ppq))
      case NoteGen(s)     => s.map(MidiValue[NoteTag])
      case DurationGen(s) => s.map(d => Tick.fromDouble(d, ppq))
      case VelocityGen(s) => s.map(MidiValue[VelocityTag])
    }

}
