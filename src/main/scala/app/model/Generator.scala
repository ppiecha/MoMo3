package app.model

import cats.data.Kleisli
import cats.syntax.all.*
import app.midi.*
import app.*

given Conversion[LazyList[Int], LazyList[Double]] with
  def apply(s: LazyList[Int]): LazyList[Double] = s.map(_.toDouble)

extension [A](ll: LazyList[A]) {
  def repeatN(n: Int): LazyList[A] = if n <= 0 then LazyList.empty else ll ++ ll.repeatN(n - 1)
}

enum Generator[A] {
  case TimeGen(s: LazyList[Double])     extends Generator[Tick]
  case NoteGen(s: LazyList[Int])        extends Generator[Note]
  case DurationGen(s: LazyList[Double]) extends Generator[Duration]

}

object Generator {

  private def doubleToTick(d: Double): App[IsValid[Tick]] =
    Kleisli { env =>
      if d == 0.0 then Right(Tick.from(0L))
      else Right(Tick.from(((env.ctx.ppq.value.toDouble * 4) / d.toDouble).toLong))
    }

  def parse[A](seq: Generator[A]): App[LazyList[IsValid[A]]] =
    Kleisli { env =>
      seq match {
        case TimeGen(s)     => s.traverse(d => doubleToTick(d).run(env))
        case NoteGen(s)     => Right(s.map(MidiValue.from))
        case DurationGen(s) => s.traverse(d => doubleToTick(d).run(env))
      }
    }
}
