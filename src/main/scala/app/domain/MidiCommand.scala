package app.domain

enum MidiCommand {
  case NoteOn(channel: Channel, note: Note, velocity: Velocity)
  case NoteOff(channel: Channel, note: Note)
  case ProgramChange(channel: Channel, bank: Bank, program: Program)
  case ControlChange(channel: Channel, control: Control, value: MidiValue)
}

object MidiCommand {
  def splitNoteOnOff(commands: List[MidiCommand]): (List[MidiCommand], Option[MidiCommand.NoteOff]) = {
    val (others, noteOff) = commands.partition {
      case NoteOff(_, _) => false
      case _ => true
    }
    (others, noteOff.headOption.map(_.asInstanceOf[NoteOff]))
  }
}
