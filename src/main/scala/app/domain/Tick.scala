package app.domain

import app.shared.*
import cats.syntax.all.*
import scala.concurrent.duration.*

opaque type Tick = Int
object Tick {
  def fromInt(value: Int): IsValid[Tick] =
    if value >= 0 then value.validNec[ValidationError]
    else ValidationError.InvalidTick(value).invalidNec[Tick]

  def fromDouble(d: Double, ppq: Ppq): IsValid[Tick] =
    val value = if d == 0.0 then 0L else ((ppq.value.toDouble * 4) / d).toLong
    if value >= 0 then Tick.fromInt(value.toInt)
    else ValidationError.InvalidTick(value.toInt).invalidNec[Tick]

  val zero: Tick = 0

  extension (tick: Tick) {
    def value: Int           = tick
    def +(other: Tick): Tick = tick + other
    def -(other: Tick): Tick = tick - other
    def toMillis(ppq: Ppq, bpm: Bpm): FiniteDuration =
      ((tick.toDouble / ppq.value.toDouble) * (60000.0 / bpm.value.toDouble)).millis
  }
}
