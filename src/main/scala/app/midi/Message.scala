package app.midi

import javax.sound.midi
import javax.sound.midi.{Sequence, ShortMessage, MidiEvent}
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import app.MidiStream
import cats.effect.*
import fs2.*
import scala.concurrent.duration.FiniteDuration

enum Message {
  case NoteMessage(command: MidiValue, channel: Channel, note: Note, velocity: Velocity)
  case ProgramMessage(channel: Channel, bank: Bank, program: Program)
  case ControlMessage(channel: Channel, control: Control, value: MidiValue)
}

object Message {
  def makeMidiMessage(command: MidiValue, channel: Channel, data1: Int, data2: Int): ShortMessage = {
    val msg = new ShortMessage()
    msg.setMessage(command.value, channel.value, data1, data2)
    msg
  }

  def makeMidiStream[F[_]: Async](message: Message): MidiStream[F] = message match {
    case Message.NoteMessage(command, channel, note, velocity) =>
      Stream(makeMidiMessage(command, channel, note.value, velocity.value))
    case Message.ProgramMessage(channel, bank, program) =>
      Stream(
        makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 0, bank.value >> 7),
        makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 32, bank.value & 0x7f),
        makeMidiMessage(MidiValue.unsafe(PROGRAM_CHANGE), channel, program.value, 0)
      )
    case Message.ControlMessage(channel, control, value) =>
      Stream(makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, control.value, value.value))
  }

  def makeMidiStream[F[_]: Async](
      channel: Channel,
      start: FiniteDuration,
      note: Note,
      duration: FiniteDuration
  ): MidiStream[F] = {
    makeMidiStream(Message.NoteMessage(MidiValue.unsafe(NOTE_ON), channel, note,  MidiValue.unsafe(100))) ++
    Stream.sleep_[F](duration) ++
    makeMidiStream(Message.NoteMessage(MidiValue.unsafe(NOTE_OFF), channel, note, MidiValue.unsafe(0)))
  }

}
