package app.domain

import cats.data.ValidatedNec
import cats.syntax.all.*

opaque type Ppq = Int
object Ppq {

  val DEFAULT_VALUE: Int = 960
  val DEFAULT_PPQ: Ppq   = 960

  def from(value: Int): ValidatedNec[ValidationError, Ppq] =
    if value > 0 then value.validNec[ValidationError]
    else ValidationError.InvalidPpq(value).invalidNec[Ppq]

  extension (ppq: Ppq) {
    def value: Int = ppq
  }
}
