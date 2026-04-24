package app.domain

enum Message {
  import Message.*
  case NoteMessage(note: Note, duration: Time, velocity: Velocity)
  case ProgramMessage(bank: Bank, program: Program)
  case ControlMessage(control: Control, value: MidiValue)

  def toCommands(channel: Channel): List[MidiCommand] = this match {
    case NoteMessage(note, duration, velocity) =>
      List(
        MidiCommand.NoteOn(channel, note, velocity),
        MidiCommand.NoteOff(channel, note)
      )
    case ProgramMessage(bank, program) =>
      List(MidiCommand.ProgramChange(channel, bank, program))
    case ControlMessage(control, value) =>
      List(MidiCommand.ControlChange(channel, control, value))

  }
}
