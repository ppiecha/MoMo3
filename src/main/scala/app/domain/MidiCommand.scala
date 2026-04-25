package app.domain

enum MidiCommand {
  case NoteOn(channel: Channel, note: Note, velocity: Velocity)
  case NoteOff(channel: Channel, note: Note)
  case ProgramChange(channel: Channel, bank: Bank, program: Program)
  case ControlChange(channel: Channel, control: Control, value: MidiValue)
}
