package app.midi

import app.config.*
import app.domain.*
import cats.syntax.all.*

opaque type MidiValue = Int
object MidiValue {
  def from(value: Int): IsValid[MidiValue] =
    if value >= 0 && value <= 127 then value.validNec[ValidationError]
    else ValidationError.InvalidMidiValue(value).invalidNec[MidiValue]

  def unsafe(value: Int): MidiValue = value

  extension (mv: MidiValue) {
    def value: Int = mv
  }
}

type Note     = MidiValue
type Velocity = MidiValue
type Bank     = MidiValue
type Program  = MidiValue
type Control  = MidiValue
