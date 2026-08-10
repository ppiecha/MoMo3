package app.domain

import cats.data.ValidatedNec
import cats.syntax.all.*

opaque type MidiValue = Int
object MidiValue {
  def from(value: Int): ValidatedNec[ValidationError, MidiValue] =
    if value >= 0 && value <= 127 then value.validNec[ValidationError]
    else ValidationError.InvalidMidiValue(value).invalidNec[MidiValue]

  extension (mv: MidiValue) {
    def value: Int = mv
  }
}

type Note    = MidiValue
type Bank    = MidiValue
type Program = MidiValue
type Control = MidiValue
