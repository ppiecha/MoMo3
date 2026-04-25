package app.domain

import app.config.*
import app.shared.*
import cats.syntax.all.*
import scala.concurrent.duration.*

opaque type Tick = Int
object Tick {
  def fromInt(value: Int): IsValid[Tick] =
    if value >= 0 then value.validNec[ValidationError]
    else ValidationError.InvalidTick(value).invalidNec[Tick]

  def fromDouble(d: Double, ppq: Ppq): IsValid[Tick] =
    val value = if d == 0.0 then 0L else ((ppq.value.toDouble * 4) / d.toDouble).toLong
    if value >= 0 then Tick.fromInt(value.toInt)
    else ValidationError.InvalidTick(value.toInt).invalidNec[Tick]

  def unsafe(value: Int): Tick = value

  val zero: Tick = 0

  extension (tick: Tick) {
    def value: Int           = tick
    def +(other: Tick): Tick = Tick.unsafe(tick + other)
    def -(other: Tick): Tick = Tick.unsafe(tick - other)
    def toMillis(env: Environment): FiniteDuration =
      ((tick.toDouble / env.ppq.value.toDouble) * (60000.0 / env.bpm.value.toDouble)).millis      
  }
}
