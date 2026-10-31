package app.domain

enum MidiCommand {
  case NoteOn(channel: Channel, note: Note, velocity: Velocity)
  case NoteOff(channel: Channel, note: Note, velocity: Velocity)
  case ProgramChange(channel: Channel, bank: Bank, program: Program)
  case ControlChange(channel: Channel, control: Control, value: Control)
  case AllNotesOff(channel: Channel)
  case AllSoundsOff(channel: Channel)
}
