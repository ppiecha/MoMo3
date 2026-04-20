package app.application

import app.shared.ErrorOr
import app.midi.Event
import javax.sound.midi.MidiEvent

final case class CompiledTrack(events: LazyList[ErrorOr[Event]]) {
  def listOfMidiEvents: LazyList[ErrorOr[MidiEvent]] =
    events.flatMap {
      case Right(event) => event.listOfMidiEvents.map(Right(_))
      case Left(error)  => LazyList(Left(error))

    }
}
