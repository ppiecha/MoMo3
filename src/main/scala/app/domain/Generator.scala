package app.domain

import cats.data.ValidatedNec
import cats.effect.*
import cats.syntax.all.*
import fs2.*

import scala.concurrent.duration.*

given Conversion[LazyList[Int], LazyList[Double]] with
  def apply(s: LazyList[Int]): LazyList[Double] = s.map(_.toDouble)

extension [A](ll: LazyList[A]) {
  def repeatN(n: Int): LazyList[A] = if n <= 0 then LazyList.empty else ll ++ ll.repeatN(n - 1)
}

enum Generator[A] {
  case TimeGen(s: LazyList[Double])     extends Generator[Tick]
  case NoteGen(s: LazyList[Int])        extends Generator[Note]
  case DurationGen(s: LazyList[Double]) extends Generator[Tick]
  case VelocityGen(s: LazyList[Int])    extends Generator[Velocity]
}

object Generator {

  def parse[A](seq: Generator[A], ppq: Ppq): LazyList[ValidatedNec[ValidationError, A]] =
    seq match {
      case TimeGen(s)     => s.map(d => Tick.fromDouble(d, ppq))
      case NoteGen(s)     => s.map(MidiValue.from)
      case DurationGen(s) => s.map(d => Tick.fromDouble(d, ppq))
      case VelocityGen(s) => s.map(Velocity.from)
    }

}
