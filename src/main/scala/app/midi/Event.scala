package app.midi

import cats.effect.*
import app.*
import fs2.*
import javax.sound.midi.*

extension (event: MidiEvent) {
  def toString2 = s"(${Message.fromShortMessage(event.getMessage.asInstanceOf[ShortMessage])}, tick: ${event.getTick})"
  def getShortMessage: ShortMessage = event.getMessage.asInstanceOf[ShortMessage]
}

case class Event(channel: Channel, message: Message, time: Time) {
  def streamOfMidiMessages[F[_]: Async]: Stream[F, ShortMessage] = {
    val midiMessages = message.toMidiMessages(channel)
    message match
      case Message.NoteMessage(_, duration, _) => {
        if midiMessages.length == 2 then
          Stream(midiMessages.head) ++ Stream.sleep_[F](duration.duration) ++ Stream(midiMessages.tail.head)
        else throw new RuntimeException("NoteMessage should produce exactly 2 MIDI messages (NOTE_ON and NOTE_OFF)")
      }
      case _ => Stream.emits(midiMessages)
  }

  val listOfMidiEvents: LazyList[MidiEvent] = {
    val midiMessages = message.toMidiMessages(channel)
    message match
      case Message.NoteMessage(_, duration, _) => {
        if midiMessages.length == 2 then
          LazyList(
            new MidiEvent(midiMessages.head, time.tick.value),
            new MidiEvent(midiMessages.tail.head, time.tick.value + duration.tick.value)
          )
        else throw new RuntimeException(s"NoteMessage should produce exactly 2 MIDI messages (NOTE_ON and NOTE_OFF) but got ${midiMessages}")
      }
      case _ => throw new RuntimeException("Only NoteMessage are supported in listOfMidiEvents for now")
  }    

}

object Event {
  def makeList(channel: Channel, time: Time, note: Note, duration: Time): LazyList[Event] = {
    val noteEvent = Event(channel, Message.NoteMessage(note, duration, MidiValue.unsafe(100)), time)
    LazyList(noteEvent)
  }
}
