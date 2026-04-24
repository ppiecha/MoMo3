package app.application

import app.shared.ErrorOr
import app.domain.Event
import javax.sound.midi.MidiEvent
import app.midi.EventConverter

final case class CompiledTrack(events: LazyList[ErrorOr[Event]]) {
  def listOfMidiEvents: LazyList[ErrorOr[MidiEvent]] =
    events.flatMap {
      case Right(event) => EventConverter.eventToListOfMidiEvents(event).map(Right(_))
      case Left(error)  => LazyList(Left(error))

    }
}
