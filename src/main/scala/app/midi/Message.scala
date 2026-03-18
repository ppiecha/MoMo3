package app.midi

import javax.sound.midi
import javax.sound.midi.{Sequence, ShortMessage, MidiEvent}
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import app.MidiStream
import cats.effect.*
import fs2.*
import scala.concurrent.duration.FiniteDuration

enum Message {
  case NoteMessage(note: Note, duration: Time, velocity: Velocity)
  case ProgramMessage(bank: Bank, program: Program)
  case ControlMessage(control: Control, value: MidiValue)
}

object Message {
  def makeMidiMessage(command: MidiValue, channel: Channel, data1: Int, data2: Int): ShortMessage = {
    val msg = new ShortMessage()
    msg.setMessage(command.value, channel.value, data1, data2)
    msg
  }

  // def makeMidiStream[F[_]: Async](message: Message): MidiStream[F] = message match {
  //   case Message.NoteMessage(channel, note, duration, velocity) =>
  //     Stream(makeMidiMessage(MidiValue.unsafe(NOTE_ON), channel, note.value, velocity.value)) ++
  //     Stream.sleep_[F](duration) ++
  //     Stream(makeMidiMessage(MidiValue.unsafe(NOTE_OFF), channel, note.value, 0))
  //   case Message.ProgramMessage(channel, bank, program) =>
  //     Stream(
  //       makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 0, bank.value >> 7),
  //       makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, 32, bank.value & 0x7f),
  //       makeMidiMessage(MidiValue.unsafe(PROGRAM_CHANGE), channel, program.value, 0)
  //     )
  //   case Message.ControlMessage(channel, control, value) =>
  //     Stream(makeMidiMessage(MidiValue.unsafe(CONTROL_CHANGE), channel, control.value, value.value))
  // }

  // def makeMidiStream[F[_]: Async](
  //     channel: Channel,
  //     start: FiniteDuration,
  //     note: Note,
  //     duration: FiniteDuration
  // ): MidiStream[F] = {
  //   val overallDuration = start + duration
  //   val noteOn  = makeMidiStream(Message.NoteMessage(MidiValue.unsafe(NOTE_ON), channel, note, MidiValue.unsafe(100)))
  //   val rest    = Stream.sleep_[F](duration)
  //   val noteOff = makeMidiStream(Message.NoteMessage(MidiValue.unsafe(NOTE_OFF), channel, note, MidiValue.unsafe(0)))
  //   if note.value == 0 || duration.toMillis == 0 /*|| velocity.value == 0*/ then rest
  //   else {
  //     println(
  //       s"Scheduling note: channel=${channel.value}, start=${start.toMillis}ms, note=${note.value}, duration=${duration.toMillis}ms"
  //     )
  //     noteOn ++ rest ++ noteOff
  //   }
  // }
}
