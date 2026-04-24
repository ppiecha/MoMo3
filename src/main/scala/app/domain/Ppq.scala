package app.domain

import app.config.*
import app.shared.*
import cats.syntax.all.*

opaque type Ppq = Int
object Ppq {
  def from(value: Int): IsValid[Ppq] =
    if value > 0 then value.validNec[ValidationError]
    else ValidationError.InvalidPpq(value).invalidNec[Ppq]

  def unsafe(value: Int): Ppq = value

  extension (ppq: Ppq) {
    def value: Int = ppq
  }
}
