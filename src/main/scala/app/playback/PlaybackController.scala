package app.playback

import app.domain.AbstractMidiEvent.{AbsoluteMidiEvent, RestEvent}
import app.domain.{AbstractMidiEvent, PlaybackPlan}
import cats.data.NonEmptyList
import cats.effect.FiberIO
import cats.effect.IO
import cats.effect.Ref
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

/** High-level playback control for start, pause, resume, stop and replacing the current plan with a freshly compiled
  * set of tracks.
  */
trait PlaybackController {
  def play(
    plan: PlaybackPlan,
    policy: RepeatPolicy
  ): IO[Unit]
  def pause: IO[Unit]
  def resume: IO[Unit]
  def stop: IO[Unit]
  def replace(
    plan: PlaybackPlan,
    policy: RepeatPolicy
  ): IO[Unit]
  def elapsedTime: IO[FiniteDuration]
  def status: IO[PlaybackStatus]
  def activePlan: IO[Option[PlaybackPlan]]
}

enum PlaybackStatus {
  case Stopped
  case Playing
  case Paused
}

object PlaybackController {
  def live(
    send: AbstractMidiEvent => IO[Unit]
  ): IO[PlaybackController] =
    Ref.of[IO, PlaybackState](PlaybackState(None, 0.millis, RepeatPolicy.none, None)).map(stateRef => new LivePlaybackController(send, stateRef))

  private[playback] def nextRepeat(
    repeatedPlan: PlaybackPlan,
    policy: RepeatPolicy
  ): Option[(PlaybackPlan, RepeatPolicy)] =
    Option.when(policy.shouldRepeat)(repeatedPlan -> policy.next)
}

private final case class PlaybackState(
  activePlan: Option[PlaybackPlan],
  elapsed: FiniteDuration,
  policy: RepeatPolicy,
  fiber: Option[FiberIO[Unit]]
)

private final class LivePlaybackController(
  send: AbstractMidiEvent => IO[Unit],
  stateRef: Ref[IO, PlaybackState]
) extends PlaybackController {

  val logger = Slf4jLogger.getLogger[IO]
  
  private val zeroDuration: FiniteDuration = FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS)

  override def pause: IO[Unit] =
    stateRef.modify { state =>
      state.fiber match {
        case Some(fiber) =>
          (
            state.copy(fiber = None),
            fiber.cancel *> PlaybackExecution.sendAllNotesOff(send) *> logger.debug("Playback paused")
          )
        case None =>
          (state, IO.unit)
      }
    }.flatten

  override def resume: IO[Unit] =
    stateRef.modify { state =>
      val action = state.activePlan match {
        case Some(plan) => start(plan, state.policy, state.elapsed)
        case None       => IO.unit
      }
      (state, action)
    }.flatten

  override def stop: IO[Unit] =
    stateRef.modify { state =>
      val cancel = state.fiber.fold(IO.unit)(_.cancel)
      (PlaybackState(None, 0.millis, RepeatPolicy.none, None), cancel *> PlaybackExecution.sendAllSoundsOff(send) *> logger.debug("Playback stopped"))
    }.flatten

  override def play(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] =
    start(plan, policy, 0.millis)

  private def start(plan: PlaybackPlan, policy: RepeatPolicy, elapsed: FiniteDuration): IO[Unit] = {
    val adjustedPlan =
      if (elapsed <= zeroDuration) plan
      else PlaybackPlanResume.resumeFrom(plan, elapsed)

    val task: IO[Unit] = repeatLoop(adjustedPlan, plan, policy, elapsed)

    task.start.flatMap { fiber =>
      stateRef.update(_.copy(activePlan = Some(plan), elapsed = elapsed, policy = policy, fiber = Some(fiber)))
    }
  }

  override def replace(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] =
    stateRef.modify { state =>
      val cancel = state.fiber.fold(IO.unit)(_.cancel)
      val initialChanges =
        if state.elapsed > zeroDuration then
          state.activePlan.fold(NonEmptyList.one(RestEvent))(PlaybackPlanDiff.changedInitialControlEvents(_, plan))
        else NonEmptyList.one(RestEvent)
      val sendInitialChanges = initialChanges.foldLeft(IO.unit)((action, event) => action *> send(event))
      (
        state.copy(fiber = None),
        cancel *> PlaybackExecution.sendAllSoundsOff(send) *> sendInitialChanges *> start(plan, policy, state.elapsed)
      )
    }.flatten

  override def elapsedTime: IO[FiniteDuration] =
    stateRef.get.map(_.elapsed)

  override def status: IO[PlaybackStatus] =
    stateRef.get.map { state =>
      if state.fiber.nonEmpty then PlaybackStatus.Playing
      else if state.activePlan.nonEmpty then PlaybackStatus.Paused
      else PlaybackStatus.Stopped
    }

  override def activePlan: IO[Option[PlaybackPlan]] =
    stateRef.get.map(_.activePlan)

  private def repeatLoop(
    initialPlan: PlaybackPlan,
    repeatedPlan: PlaybackPlan,
    policy: RepeatPolicy,
    baselineElapsed: FiniteDuration
  ): IO[Unit] = {
    def loop(
      currentPlan: PlaybackPlan,
      currentPolicy: RepeatPolicy,
      currentBaselineElapsed: FiniteDuration
    ): IO[Unit] =
      PlaybackExecution
        .executeWithProgress(
          currentPlan,
          send,
          progress => stateRef.update(_.copy(elapsed = currentBaselineElapsed + progress))
        )
        .flatMap { _ =>
          PlaybackController.nextRepeat(repeatedPlan, currentPolicy) match {
            case Some((nextPlan, nextPolicy)) =>
              loop(nextPlan, nextPolicy, 0.millis)
            case None =>
              stateRef.update(_.copy(elapsed = currentBaselineElapsed, fiber = None, activePlan = None)).void
          }
        }

    loop(initialPlan, policy, baselineElapsed)
  }
}
