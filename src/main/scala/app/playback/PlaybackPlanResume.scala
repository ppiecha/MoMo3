package app.playback

import app.domain.{PlaybackPlan, TimedEvent}
import scala.concurrent.duration.FiniteDuration

/** Utility methods for rebuilding a playback plan starting from a previously elapsed moment in time.
  */
object PlaybackPlanResume {

  /** Drops all events that were already played and keeps only the remaining tail.
    *
    * If the current time falls in the middle of an event delay, the first remaining event is shortened to the remaining
    * delay. This is how a playback can resume smoothly without replaying the already-elapsed portion.
    */
  def resumeFrom(plan: PlaybackPlan, elapsed: FiniteDuration): PlaybackPlan = {
    if (elapsed <= FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS)) {
      return plan
    }

    @annotation.tailrec
    def loop(
      remaining: FiniteDuration,
      events: Vector[TimedEvent],
      acc: Vector[TimedEvent]
    ): Vector[TimedEvent] =
      events match {
        case Vector() => acc
        case head +: tail =>
          if (remaining >= head.delay) {
            loop(remaining - head.delay, tail, acc)
          } else {
            val shortened = TimedEvent(head.delay - remaining, head.event)
            (acc :+ shortened) ++ tail
          }
      }

    PlaybackPlan(loop(elapsed, plan.events.toVector, Vector.empty))
  }
}
