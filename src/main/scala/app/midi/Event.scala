package app.midi

import cats.effect.*
import app.*
import fs2.*
import javax.sound.midi.*

case class Event(channel: Channel, message: Message, time: Time) {
  def streamOfMidiMessages[F[_]: Async]: Stream[F, MidiMessage] = {
    val midiMessages = message.toMidiMessages(channel)
    message match
      case Message.NoteMessage(_, duration, _) => {
        if midiMessages.length == 2 then
          Stream(midiMessages.head) ++ Stream.sleep_[F](duration.duration) ++ Stream(midiMessages.tail.head)
        else throw new RuntimeException("NoteMessage should produce exactly 2 MIDI messages (NOTE_ON and NOTE_OFF)")
      }
      case _ => Stream.emits(midiMessages)
  }

  val listOfMidiEvents: LazyList[MidiEvent] = 
    LazyList(message.toMidiMessages(channel).map { midiMessage =>  new MidiEvent(midiMessage, time.tick.value) }: _*)

}

object Event {
  def makeList(channel: Channel, time: Time, note: Note, duration: Time): LazyList[Event] = {
    val noteEvent = Event(channel, Message.NoteMessage(note, duration, MidiValue.unsafe(100)), time)
    LazyList(noteEvent)
  }
}
