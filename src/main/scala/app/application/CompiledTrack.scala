package app.application

import app.midi.*
import javax.sound.midi.MidiEvent
import app.config.ErrorOr

final case class CompiledTrack(events: LazyList[ErrorOr[Event]]) {
  def listOfMidiEvents: LazyList[ErrorOr[MidiEvent]] =
    events.flatMap {
      case Right(event) => event.listOfMidiEvents.map(Right(_))
      case Left(error)  => LazyList(Left(error))

    }
}
