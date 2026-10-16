package app.domain

import cats.data.ValidatedNec
import cats.syntax.all._

type ValidatedBpm = ValidatedNec[DomainError, Bpm]

opaque type Bpm = Int
object Bpm {
  def from(value: Int): ValidatedBpm =
    if value > 0 then value.validNec[DomainError]
    else DomainError.InvalidBpm(value).invalidNec[Bpm]

  extension (bpm: Bpm) {
    def value: Int = bpm
  }
}
