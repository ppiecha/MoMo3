package app.domain

import cats.data.ValidatedNec
import cats.syntax.all.*

opaque type Channel = Int

object Channel {
  def from(value: Int): ValidatedNec[ValidationError, Channel] =
    if value >= 0 && value <= 15 then value.validNec[ValidationError]
    else ValidationError.InvalidChannel(value).invalidNec[Channel]

  extension (ch: Channel) {
    def value: Int = ch
  }
}
