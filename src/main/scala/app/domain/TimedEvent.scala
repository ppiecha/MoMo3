package app.domain

import scala.concurrent.duration.FiniteDuration

case class TimedEvent(delay: FiniteDuration, event: AbsoluteMidiEvent)

object TimedEvent {

  private def commandOrder(command: MidiCommand): Int =
    command match {
      case MidiCommand.ControlChange(_, _, _) => 0
      case MidiCommand.ProgramChange(_, _, _) => 1
      case MidiCommand.NoteOff(_, _, _)       => 2
      case MidiCommand.NoteOn(_, _, _)        => 3
    }

  def fromAbsoluteEvents(events: Seq[AbsoluteMidiEvent], timingContext: TimingContext): Seq[TimedEvent] = {
    events
      .sortBy(event => (event.at.value, commandOrder(event.command)))
      .foldLeft((Tick.zero, Vector.empty[(Tick, AbsoluteMidiEvent)])) { case ((prev, acc), e) =>
        val delta = e.at - prev
        (e.at, acc :+ (delta -> e))
      }
      ._2
      .map { case (tick, event) => TimedEvent(tick.toMillis(timingContext.ppq, timingContext.bpm), event) }
  }
}
