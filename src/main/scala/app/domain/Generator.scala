package app.domain

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import app.midi.*
import app.config.*
import scala.concurrent.duration.*

given Conversion[LazyList[Int], LazyList[Double]] with
  def apply(s: LazyList[Int]): LazyList[Double] = s.map(_.toDouble)

extension [A](ll: LazyList[A]) {
  def repeatN(n: Int): LazyList[A] = if n <= 0 then LazyList.empty else ll ++ ll.repeatN(n - 1)
}

enum Generator[A] {
  case TimeGen(s: LazyList[Double])     extends Generator[Time]
  case NoteGen(s: LazyList[Int])        extends Generator[Note]
  case DurationGen(s: LazyList[Double]) extends Generator[Time]
}

object Generator {

  def doubleToFiniteDuration(d: Double, env: Environment): IsValid[FiniteDuration] =
    val value = (4 * 60000 / (d * env.bpm.value)).toLong
    if value >= 0 then value.millis.validNec[ValidationError]
    else ValidationError.InvalidTimeValue(value).invalidNec[FiniteDuration]

  def doubleToTick(d: Double, env: Environment): IsValid[Tick] =
    val value = if d == 0.0 then 0L else ((env.ppq.value.toDouble * 4) / d.toDouble).toLong
    if value >= 0 then Tick.from(value.toInt)
    else ValidationError.InvalidTick(value.toInt).invalidNec[Tick]

  def doubleToTime(d: Double, env: Environment): IsValid[Time] =
    (doubleToFiniteDuration(d, env), doubleToTick(d, env)).mapN(Time.apply)

  def parse[A](seq: Generator[A], env: Environment): LazyList[IsValid[A]] =
    seq match {
      case TimeGen(s)     => s.map(d => doubleToTime(d, env))
      case NoteGen(s)     => s.map(MidiValue.from)
      case DurationGen(s) => s.map(d => doubleToTime(d, env))
    }
}
