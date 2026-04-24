package app.midi

import app.domain.MidiCommand
import MidiCommand.*
import javax.sound.midi.ShortMessage

extension (mc: MidiCommand) {
  def toMidiMessages: List[ShortMessage] = mc match
    case NoteOn(channel, note, velocity) =>
      List(new ShortMessage(ShortMessage.NOTE_ON, channel.value, note.value, velocity.value))
    case NoteOff(channel, note) =>
      List(new ShortMessage(ShortMessage.NOTE_OFF, channel.value, note.value, 0))
    case ProgramChange(channel, bank, program) =>
      List(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel.value, program.value, 0))
    case ControlChange(channel, controlNumber, controlValue) =>
      List(new ShortMessage(ShortMessage.CONTROL_CHANGE, channel.value, controlNumber.value, controlValue.value))
}
