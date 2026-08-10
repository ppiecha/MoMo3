package app.domain

import cats.data.ValidatedNec
import cats.syntax.all.*

opaque type Bpm = Int
object Bpm {
  def from(value: Int): ValidatedNec[ValidationError, Bpm] =
    if value > 0 then value.validNec[ValidationError]
    else ValidationError.InvalidBpm(value).invalidNec[Bpm]

  extension (bpm: Bpm) {
    def value: Int = bpm
  }
}
