package app.domain
import app.syntax.Extensions.repeat
import cats.data.NonEmptyList

sealed trait TickGenerator { def values: NonEmptyList[Double] }

final case class TimeGen(values: NonEmptyList[Double]) extends TickGenerator {
  def repeat(n: Int): TimeGen = TimeGen(values.repeat(n))
  def length: Int             = values.length
  def duration: Double        = if length == 0 then 0 else values.map(v => 1d / v).toList.sum
}

final case class DurationGen(values: NonEmptyList[Double]) extends TickGenerator {
  def repeat(n: Int): DurationGen = DurationGen(values.repeat(n))
  def length: Int                 = values.length
}

object TickGenerator {
  def time(head: Double, tail: Double*): TimeGen         = TimeGen(NonEmptyList(head, tail.toList))
  def duration(head: Double, tail: Double*): DurationGen = DurationGen(NonEmptyList(head, tail.toList))
}
