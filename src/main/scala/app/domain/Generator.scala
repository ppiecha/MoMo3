package app.domain

import cats.data.ValidatedNec

enum Generator[A] {
  case TimeGen(s: Seq[Double])     extends Generator[Tick]
  case NoteGen(s: Seq[Int])        extends Generator[Note]
  case DurationGen(s: Seq[Double]) extends Generator[Tick]
  case VelocityGen(s: Seq[Int])    extends Generator[Velocity]
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
