package app.domain

final case class AbsoluteMidiEvent(at: Tick, command: MidiCommand)
