package app.domain

import cats.data.ValidatedNec
import cats.syntax.all.*

type ValidatedBpm = ValidatedNec[ValidationError, Bpm]

opaque type Bpm = Int
object Bpm {
  def from(value: Int): ValidatedBpm =
    if value > 0 then value.validNec[ValidationError]
    else ValidationError.InvalidBpm(value).invalidNec[Bpm]

  extension (bpm: Bpm) {
    def value: Int = bpm
  }
}
