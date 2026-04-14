package app.midi

import app.*
import app.ValidationError
import cats.syntax.all.*

opaque type Tick = Int
object Tick {
  def from(value: Int): IsValid[Tick] =
    if value >= 0 then value.validNec[ValidationError]
    else ValidationError.InvalidTick(value).invalidNec[Tick]

  def unsafe(value: Int): Tick = value

  extension (tick: Tick) {
    def value: Int           = tick
    def +(other: Tick): Tick = Tick.unsafe(tick + other)
  }
}
