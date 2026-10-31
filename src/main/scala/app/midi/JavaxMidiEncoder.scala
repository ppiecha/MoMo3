package app.midi

import app.domain.MidiCommand

import javax.sound.midi.ShortMessage

import MidiCommand.*

extension (mc: MidiCommand) {
  def toMidiMessages: List[ShortMessage] = mc match
    case NoteOn(channel, note, velocity) =>
      List(new ShortMessage(ShortMessage.NOTE_ON, channel.value, note.value, velocity.value))
    case NoteOff(channel, note, velocity) =>
      List(new ShortMessage(ShortMessage.NOTE_OFF, channel.value, note.value, 0))
    case AllNotesOff(channel) =>
      List(new ShortMessage(ShortMessage.CONTROL_CHANGE, channel.value, 123, 0))
    case AllSoundsOff(channel) =>
      List(new ShortMessage(ShortMessage.CONTROL_CHANGE, channel.value, 120, 0))
    case ProgramChange(channel, bank, program) =>
      val msb = bank.value // / 128
      val lsb = 0          // bank.value % 128

      List(
        new ShortMessage(ShortMessage.CONTROL_CHANGE, channel.value, 0, msb),          // Bank Select MSB (CC 0)
        new ShortMessage(ShortMessage.CONTROL_CHANGE, channel.value, 32, lsb),         // Bank Select LSB (CC 32)
        new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel.value, program.value, 0) // Program Change
      )
    case ControlChange(channel, controlNumber, controlValue) =>
      List(new ShortMessage(ShortMessage.CONTROL_CHANGE, channel.value, controlNumber.value, controlValue.value))
}
