package app.domain

import app.shared.*

opaque type Velocity = MidiValue
object Velocity {
  def from(value: Int): IsValid[Velocity] =
    MidiValue.from(value) // .map(_.asInstanceOf[Velocity])
}
