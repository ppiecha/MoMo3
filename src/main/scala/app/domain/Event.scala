package app.domain

import app.*

case class Event(channel: Channel, message: GeneratedMessage, time: Time)

object Event {
  def apply(channel: Channel, time: Time, note: Note, duration: Time): Event = {
    Event(
      channel, 
      GeneratedMessage(
        Some(Message.NoteMessage(note, duration, MidiValue.unsafe(127))),
        None,
        None
      ),
      time
    )
  }
}
