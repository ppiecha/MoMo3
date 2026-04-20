package app.midi

import cats.effect.*
import app.*
import fs2.*
import javax.sound.midi.*

extension (event: MidiEvent) {
  def toString2 = s"(${Message.fromShortMessage(event.getMessage.asInstanceOf[ShortMessage])}, tick: ${event.getTick})"
  def getShortMessage: ShortMessage = event.getMessage.asInstanceOf[ShortMessage]
}

case class Event(channel: Channel, message: GeneratedMessage, time: Time) {

  def streamOfMidiMessages[F[_]: Async]: Stream[F, ShortMessage] = {
    val List(noteOn, noteOff) = message.toMidiMessages(channel)
    Stream(noteOn) ++ Stream.sleep_[F](message.note.duration.duration) ++ Stream(noteOff)
  }

  val listOfMidiEvents: LazyList[MidiEvent] = {
    val List(noteOn, noteOff) = message.toMidiMessages(channel)
    LazyList(
      new MidiEvent(noteOn, time.tick.value),
      new MidiEvent(noteOff, time.tick.value + message.note.duration.tick.value)
    )
  } 

  override def toString: String = s"Event(channel: $channel, messages: $message, time: ${time})"

}

object Event {
  def apply(channel: Channel, time: Time, note: Note, duration: Time): Event = {
    Event(
      channel, 
      GeneratedMessage(
        Message.NoteMessage(note, duration, MidiValue.unsafe(127)),
        None,
        None
      ),
      time
    )
  }
}
