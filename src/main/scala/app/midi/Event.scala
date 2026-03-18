package app.midi

import cats.effect.*
import app.*
import fs2.*

case class Event(channel: Channel, message: Message, time: Time)

object Event {
  def makeStream[F[_]: Async](channel: Channel, time: Time, note: Note, duration: Time): EventStream[F] = {
    val noteEvent = Event(channel, Message.NoteMessage(note, duration, MidiValue.unsafe(100)), time)
    Stream(noteEvent)
  }
}
