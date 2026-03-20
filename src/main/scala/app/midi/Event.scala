package app.midi

import cats.effect.*
import app.*
import fs2.*
import javax.sound.midi.*

case class Event(channel: Channel, message: Message, time: Time) {
  def streamOfMidiMessages[F[_]: Async]: MidiMesageStream[F] = {
    val midiMessages = message.toMidiMessages(channel)
    message match
      case Message.NoteMessage(_, _, _) => {
        if midiMessages.length == 2 then
          Stream(midiMessages.head) ++ Stream.sleep_[F](time.duration) ++ Stream(midiMessages.tail.head)
        else throw new RuntimeException("NoteMessage should produce exactly 2 MIDI messages (NOTE_ON and NOTE_OFF)")
      }
      case _ => Stream.emits(midiMessages)
  }

  val streamOfMidiEvents: Stream[Pure, MidiEvent] = ???
}

object Event {
  def makeStream[F[_]: Async](channel: Channel, time: Time, note: Note, duration: Time): EventStream[F] = {
    val noteEvent = Event(channel, Message.NoteMessage(note, duration, MidiValue.unsafe(100)), time)
    Stream(noteEvent)
  }
}
