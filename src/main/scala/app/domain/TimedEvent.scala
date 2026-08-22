package app.domain

import scala.concurrent.duration.FiniteDuration

case class TimedEvent(delay: FiniteDuration, event: AbsoluteMidiEvent)

object TimedEvent {
  def fromAbsoluteEvents(events: Seq[AbsoluteMidiEvent], timingContext: TimingContext): Seq[TimedEvent] = {
    events
      .sortBy(_.at.value)
      .foldLeft((Tick.zero, Vector.empty[(Tick, AbsoluteMidiEvent)])) { case ((prev, acc), e) =>
        val delta = e.at - prev
        (e.at, acc :+ (delta -> e))
      }
      ._2
      .map { case (tick, event) => TimedEvent(tick.toMillis(timingContext.ppq, timingContext.bpm), event) }
  }
}
