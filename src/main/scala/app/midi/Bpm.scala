package app.midi

import app.*
import app.ValidationError
import cats.syntax.all.*

opaque type Bpm = Int
object Bpm {
  def from(value: Int): IsValid[Bpm] =
    if value > 0 then value.validNec[ValidationError]
    else ValidationError.InvalidBpm(value).invalidNec[Bpm]

  def unsafe(value: Int): Bpm = value

  extension (bpm: Bpm) {
    def value: Int = bpm
  }
}
