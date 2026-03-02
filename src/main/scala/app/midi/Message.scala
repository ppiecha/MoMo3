package app.midi

import javax.sound.midi
import javax.sound.midi.{Sequence, ShortMessage, MidiEvent}
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}

enum Message {
  case NoteMessage(command: MidiValue, channel: Channel, note: Note, velocity: Velocity)
  case ProgramMessage(channel: Channel, bank: Bank, program: Program)
  case ControlMessage(channel: Channel, control: Control, value: MidiValue)
}

object Message {
  def midiMessage(command: MidiValue, channel: Channel, data1: Int, data2: Int): ShortMessage = {
    val msg = new ShortMessage()
    msg.setMessage(command.value, channel.value, data1, data2)
    msg
  }

  def makeMidiMessages(message: Message): LazyList[ShortMessage] = message match {
    case Message.NoteMessage(command, channel, note, velocity) =>
      LazyList(midiMessage(command, channel, note.value, velocity.value))
    case Message.ProgramMessage(channel, bank, program) =>
      LazyList(
        midiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 0, bank.value >> 7),
        midiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 32, bank.value & 0x7f),
        midiMessage(MidiValue.unsafe(PROGRAM_CHANGE), channel, program.value, 0)
      )
    case Message.ControlMessage(channel, control, value) =>
      LazyList(midiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, control.value, value.value))
  }
}
