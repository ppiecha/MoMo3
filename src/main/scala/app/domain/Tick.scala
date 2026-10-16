package app.domain

import cats.data.ValidatedNec
import cats.syntax.all._

import scala.concurrent.duration._

opaque type Tick = Int
object Tick {
  def fromInt(value: Int): ValidatedNec[DomainError, Tick] =
    if value >= 0 then value.validNec[DomainError]
    else DomainError.InvalidTick(value).invalidNec[Tick]

  def fromDouble(d: Double, ppq: Ppq): ValidatedNec[DomainError, Tick] =
    val value = if d == 0.0 then 0L else ((ppq.value.toDouble * 4) / d).toLong
    if value >= 0 then Tick.fromInt(value.toInt)
    else DomainError.InvalidTick(value.toInt).invalidNec[Tick]

  val zero: Tick = 0

  extension (tick: Tick) {
    def value: Int           = tick
    def +(other: Tick): Tick = tick + other
    def -(other: Tick): Tick = tick - other
    def toMillis(ppq: Ppq, bpm: Bpm): FiniteDuration =
      ((tick.toDouble / ppq.value.toDouble) * (60000.0 / bpm.value.toDouble)).millis
  }
}
