package app.domain

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import app.midi.*
import app.config.*
import app.shared.*
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
}

object Generator {

  def parse[A](seq: Generator[A], env: Environment): LazyList[IsValid[A]] =
    seq match {
      case TimeGen(s)     => s.map(d => Tick.fromDouble(d, env.ppq))
      case NoteGen(s)     => s.map(MidiValue.from)
      case DurationGen(s) => s.map(d => Tick.fromDouble(d, env.ppq))
    }

}
