package app.domain

import app.shared.*
import cats.syntax.all.*

opaque type Channel = Int

object Channel {
  def from(value: Int): IsValid[Channel] =
    if value >= 0 && value <= 15 then value.validNec[ValidationError]
    else ValidationError.InvalidChannel(value).invalidNec[Channel]

  extension (ch: Channel) {
    def value: Int = ch
  }
}
