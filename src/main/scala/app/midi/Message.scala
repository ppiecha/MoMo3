package app.midi

import javax.sound.midi
import javax.sound.midi.*
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import cats.effect.*
import fs2.*
import scala.concurrent.duration.FiniteDuration

enum Message {
  import Message.*
  case NoteMessage(note: Note, duration: Time, velocity: Velocity)
  case ProgramMessage(bank: Bank, program: Program)
  case ControlMessage(control: Control, value: MidiValue) 

  def toMidiMessages(channel: Channel): Seq[ShortMessage] = this match {
    case NoteMessage(note, duration, velocity) =>
      Seq(
        makeMidiMessage(MidiValue.unsafe(NOTE_ON), channel, note.value, velocity.value),
        makeMidiMessage(MidiValue.unsafe(NOTE_OFF), channel, note.value, 0)
      )
    case ProgramMessage(bank, program) =>
      Seq(
        makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 0, bank.value >> 7),
        makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 32, bank.value & 0x7f),
        makeMidiMessage(MidiValue.unsafe(PROGRAM_CHANGE), channel, program.value, 0)
      )
    case ControlMessage(control, value) =>
      Seq(makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, control.value, value.value))

  }
}

object Message {
  def makeMidiMessage(command: MidiValue, channel: Channel, data1: Int, data2: Int): ShortMessage = {
    val msg = new ShortMessage()
    msg.setMessage(command.value, channel.value, data1, data2)
    msg
  }

  def fromShortMessage(msg: ShortMessage): String = {
    msg.getCommand match {
      case NOTE_ON =>
        s"Note On: ${MidiValue.unsafe(msg.getData1)} Channel: ${msg.getChannel}"
      case NOTE_OFF =>
        s"Note Off: ${MidiValue.unsafe(msg.getData1)} Channel: ${msg.getChannel}"
      case CONTROL_CHANGE =>
        s"Control Change: ${MidiValue.unsafe(msg.getData1)} Value: ${MidiValue.unsafe(msg.getData2)} Channel: ${msg.getChannel}"
      case PROGRAM_CHANGE =>
        s"Program Change: ${MidiValue.unsafe(msg.getData1)} Channel: ${msg.getChannel}"
      case _ =>
        throw new RuntimeException(s"Unsupported MIDI command: ${msg.getCommand}")
    }
  }

}