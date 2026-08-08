package app.domain

import app.shared.*
import cats.implicits.catsSyntaxValidatedIdBinCompat0

opaque type Velocity = Int
object Velocity {

  val DEFAULT_VALUE = 100

  def from(value: Int): IsValid[Velocity] =
    if value >= 0 && value <= 127 then value.validNec[ValidationError]
    else ValidationError.InvalidVelocity(value).invalidNec[Velocity]

  def infinityFromDefault: LazyList[Int] = LazyList.continually(DEFAULT_VALUE)

  extension (v: Velocity) {
    def value: Int = v
  }
}
