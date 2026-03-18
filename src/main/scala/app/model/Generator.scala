package app.model

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import app.midi.*
import app.*
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

  def doubleToFiniteDuration[F[_]: Async](d: Double): App[F, IsValid[FiniteDuration]] =
    for {
      env <- ask[F]
      value = (4 * 60000 / (d * env.bpm.value)).toLong
      time <- if value >= 0 then liftFPure(value.millis.validNec[ValidationError])
      else liftFPure(ValidationError.InvalidTimeValue(value).invalidNec[FiniteDuration])
    } yield time

  def doubleToTick[F[_]: Async](d: Double): App[F, IsValid[Tick]] =
    for {
      env <- ask[F]
      value = if d == 0.0 then 0L else ((env.ppq.value.toDouble * 4) / d.toDouble).toLong
      tick <- if value >= 0 then liftFPure(Tick.from(value.toInt))
      else liftFPure(ValidationError.InvalidTick(value.toInt).invalidNec[Tick])
    } yield tick 

  def doubleToTime[F[_]: Async](d: Double): App[F, IsValid[Time]] =
    (doubleToFiniteDuration(d), doubleToTick(d)).mapN((x, y) => (x, y).mapN(Time.apply))  

  def parse[F[_]: Async, A](seq: Generator[A]): App[F, Stream[Pure, IsValid[A]]] =
    seq match {
      case TimeGen(s)     => s.traverse(doubleToTime).map(Stream.emits(_))
      case NoteGen(s)     => liftFPure(Stream.emits(s.map(MidiValue.from)))
      case DurationGen(s) => s.traverse(doubleToTime).map(Stream.emits(_))
    }
}
