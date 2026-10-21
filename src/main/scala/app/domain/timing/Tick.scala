package app.domain

import cats.data.ValidatedNec
import cats.syntax.all._

import scala.concurrent.duration._

opaque type Tick = Int
object Tick {
  private def from(value: Int): ValidatedNec[DomainError, Tick] =
    if value >= 0 then value.validNec[DomainError]
    else DomainError.InvalidTick(value).invalidNec[Tick]

  def from(d: Double)(using timingContext: TimingContext): ValidatedNec[DomainError, Tick] =
    val value = if d == 0.0 then 0L else ((timingContext.ppq.value.toDouble * 4) / d).toLong
    if value >= 0 then Tick.from(value.toInt)
    else DomainError.InvalidTick(value.toInt).invalidNec[Tick]

  val Zero: Tick = 0

  extension (tick: Tick) {
    def value: Int           = tick
    def +(other: Tick): Tick = tick + other
    def -(other: Tick): Tick = tick - other
    def toMillis(using timingContext: TimingContext): FiniteDuration =
      ((tick.toDouble / timingContext.ppq.value.toDouble) * (60000.0 / timingContext.bpm.value.toDouble)).millis
  }
}
