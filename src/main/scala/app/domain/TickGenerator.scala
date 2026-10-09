package app.domain
import app.syntax.Extensions.repeat

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

object TickGenerator {
  def time(values: Double*): TimeGen         = TimeGen(values)
  def duration(values: Double*): DurationGen = DurationGen(values)
}