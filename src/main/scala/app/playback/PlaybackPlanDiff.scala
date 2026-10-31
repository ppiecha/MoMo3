package app.playback

import app.domain.AbstractMidiEvent
import app.domain.AbstractMidiEvent.AbsoluteMidiEvent
import app.domain.AbstractMidiEvent.RestEvent
import app.domain.MidiCommand
import app.domain.PlaybackPlan
import app.domain.PlaybackPlan.PlaybackPlanData
import app.domain.Tick
import cats.data.NonEmptyList

object PlaybackPlanDiff {

  def changedInitialControlEvents(previous: PlaybackPlan, next: PlaybackPlan): NonEmptyList[AbstractMidiEvent] = {
    val previousCounts = previous match {
      case PlaybackPlan.EmptyPlaybackPlan => Map.empty[AbstractMidiEvent, Int]
      case previous: PlaybackPlan.PlaybackPlanData =>
        initialControlEvents(previous).groupMapReduce(identity)(_ => 1)(_ + _)
    }

    next match {
      case PlaybackPlan.EmptyPlaybackPlan => NonEmptyList.one(RestEvent)
      case next: PlaybackPlan.PlaybackPlanData =>
        initialControlEvents(next)
          .foldLeft((previousCounts, Vector.empty[AbstractMidiEvent])) { case ((remaining, changes), event) =>
            remaining.get(event) match {
              case Some(count) if count > 0 =>
                (remaining.updated(event, count - 1), changes)
              case _ =>
                (remaining, changes :+ event)
            }
          }
          ._2
          .toList match {
          case Nil          => NonEmptyList.one(RestEvent)
          case head +: tail => NonEmptyList(head, tail)
        }
    }
  }

  private def initialControlEvents(plan: PlaybackPlanData): Vector[AbsoluteMidiEvent] = {
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
}
