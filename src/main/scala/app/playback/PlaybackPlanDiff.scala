package app.playback

import app.domain.AbsoluteMidiEvent
import app.domain.MidiCommand
import app.domain.PlaybackPlan
import app.domain.Tick

object PlaybackPlanDiff {

  def changedInitialControlEvents(previous: PlaybackPlan, next: PlaybackPlan): Vector[AbsoluteMidiEvent] = {
    val previousCounts = initialControlEvents(previous).groupMapReduce(identity)(_ => 1)(_ + _)

    initialControlEvents(next).foldLeft((previousCounts, Vector.empty[AbsoluteMidiEvent])) {
      case ((remaining, changes), event) =>
        remaining.get(event) match {
          case Some(count) if count > 0 =>
            (remaining.updated(event, count - 1), changes)
          case _ =>
            (remaining, changes :+ event)
        }
    }._2
  }

  private def initialControlEvents(plan: PlaybackPlan): Vector[AbsoluteMidiEvent] =
    plan.events.iterator
      .map(_.event)
      .filter(_.at == Tick.Zero)
      .filterNot(event =>
        event.command match {
          case MidiCommand.NoteOn(_, _, _)  => true
          case MidiCommand.NoteOff(_, _, _) => true
          case _                            => false
        }
      )
      .toVector
}