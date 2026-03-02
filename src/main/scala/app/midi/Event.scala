package app.midi

import javax.sound.midi
import javax.sound.midi.{Sequence, ShortMessage, MidiEvent}
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import app.*
import app.midi.Message.*
import scala.concurrent.duration.FiniteDuration
import cats.data.Kleisli

enum Event(val midiMessage: Message, val tick: Tick) {
  case NoteEvent(noteMessage: Message.NoteMessage, time: Tick)          extends Event(noteMessage, time)
  case ProgramEvent(programMessage: Message.ProgramMessage, time: Tick) extends Event(programMessage, time)
  case ControlEvent(controlMessage: Message.ControlMessage, time: Tick) extends Event(controlMessage, time)
}

object Event {
  def makeMidiEvent(event: Event): LazyList[MidiEvent] =
    makeMidiMessages(event.midiMessage).map { msg =>
      new MidiEvent(msg, event.tick.value)
    }

  def makeMidiEvents(channel: Channel, start: Tick, note: Note, duration: Duration): LazyList[Event] = {
    // add program and control events if not zero
    LazyList(
      Event.NoteEvent(
        Message.NoteMessage(
          command = MidiValue.unsafe(NOTE_ON),
          channel = channel,
          note = note,
          velocity = MidiValue.unsafe(100)
        ),
        start
      ),
      Event.NoteEvent(
        Message.NoteMessage(
          command = MidiValue.unsafe(NOTE_OFF),
          channel = channel,
          note = note,
          velocity = MidiValue.unsafe(0)
        ),
        start + duration
      )
    )
  }

//   def attempt(events: LazyList[IsValid[Event]]): ErrorOr[LazyList[Event]] =
//     val (errors, validEvents) = events.partitionMap {
//       case Valid(event) => Right(event)
//       case Invalid(nec) => Left(ValidationError.InvalidEvent(s"${nec.toList.mkString(", ")}"))
//     }
//     if errors.nonEmpty then
//       Left(ValidationError.InvalidEvents(errors.toList.map(_.asInstanceOf[ValidationError.InvalidEvent])))
//     else Right(validEvents)

  def limitEvents(events: ErrorOr[LazyList[Event]], duration: Option[FiniteDuration]): App[LazyList[Event]] =
    Kleisli { env =>
      events.map(
        _.takeWhile(event =>
          if duration.isDefined then tickToSecond(event.tick, env.ctx) < duration.get.toSeconds else true
        )
      )
    }
}
