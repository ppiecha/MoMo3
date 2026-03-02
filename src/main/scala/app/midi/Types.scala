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

opaque type Channel = Int

object Channel {
  def from(value: Int): IsValid[Channel] =
    if value >= 0 && value <= 15 then value.validNec[ValidationError]
    else ValidationError.InvalidChannel(value).invalidNec[Channel]

  def unsafe(value: Int): Channel = value

  extension (ch: Channel) {
    def value: Int = ch
  }
}

opaque type Tick = Long

object Tick {
  def from(value: Long): IsValid[Tick] =
    if value >= 0 then value.validNec[ValidationError]
    else ValidationError.InvalidTick(value).invalidNec[Tick]

  def unsafe(value: Long): Tick = value

  extension (tk: Tick) {
    def value: Long          = tk
    def +(other: Tick): Tick = tk + other
  }
}

type Duration = Tick
