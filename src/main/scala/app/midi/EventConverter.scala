package app.midi

import cats.effect.*
import fs2.*
import javax.sound.midi.*
import app.domain.{Event, Time, Tick, Channel, Message}
import app.domain.MidiCommand
import MidiCommand.*

object EventConverter {

  def eventToStreamOfMidiMessages[F[_]: Async](event: Event): Stream[F, ShortMessage] = {
    val commands           = event.message.toCommands(event.channel)
    val (rest, noteOffOpt) = MidiCommand.splitNoteOnOff(commands)
    if noteOffOpt.isEmpty then Stream.emits(commands.flatMap(_.toMidiMessages))
    else
      Stream.emits(rest.flatMap(_.toMidiMessages)) ++
        Stream.sleep_[F](event.message.note.get.duration.duration) ++
        Stream.emits(noteOffOpt.get.toMidiMessages)
  }

  def eventToListOfMidiEvents(event: Event): LazyList[MidiEvent] = {
    val commands = event.message.toCommands(event.channel)
    commands
      .flatMap {
        case cmd @ NoteOff(channel, note) =>
          cmd.toMidiMessages.map(m =>
            new MidiEvent(m, event.time.tick.value + event.message.note.get.duration.tick.value)
          )
        case cmd => cmd.toMidiMessages.map(m => new MidiEvent(m, event.time.tick.value))
      }
      .to(LazyList)
  }

}
