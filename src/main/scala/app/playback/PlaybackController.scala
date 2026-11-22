package app.playback

import app.domain.AbstractMidiEvent
import app.domain.AbstractMidiEvent.RestEvent
import app.domain.PlaybackPlan
import cats.data.NonEmptyList
import cats.effect.Deferred
import cats.effect.FiberIO
import cats.effect.IO
import cats.effect.Ref
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
    Ref
      .of[IO, PlaybackState](PlaybackState(None, 0.millis, RepeatPolicy.none, None, None))
      .map(stateRef => new LivePlaybackController(send, stateRef))

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
  fiber: Option[FiberIO[Unit]],
  startedAt: Option[FiniteDuration]
)

private final class LivePlaybackController(
  send: AbstractMidiEvent => IO[Unit],
  stateRef: Ref[IO, PlaybackState]
) extends PlaybackController {

  val logger = Slf4jLogger.getLogger[IO]

  private val zeroDuration: FiniteDuration = FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS)

  private def elapsedAt(state: PlaybackState, now: FiniteDuration): FiniteDuration =
    state.elapsed + state.startedAt.fold(zeroDuration)(start => (now - start).max(zeroDuration))

  private def adjustedElapsed(
    elapsed: FiniteDuration,
    previous: Option[PlaybackPlan],
    next: PlaybackPlan
  ): FiniteDuration =
    (previous, next) match {
      case (Some(old: PlaybackPlan.PlaybackPlanData), updated: PlaybackPlan.PlaybackPlanData) =>
        (old.timing, updated.timing) match {
          case (Some(oldTiming), Some(newTiming)) =>
            // Equal beat positions correspond to durations inversely proportional to BPM.
            FiniteDuration(
              (elapsed.toNanos.toDouble * oldTiming.bpm.value / newTiming.bpm.value).toLong,
              scala.concurrent.duration.NANOSECONDS
            )
          case _ => elapsed
        }
      case _ => elapsed
    }

  override def pause: IO[Unit] =
    IO.monotonic.flatMap { now =>
      stateRef.modify { state =>
        state.fiber match {
          case Some(fiber) =>
            (
              state.copy(elapsed = elapsedAt(state, now), fiber = None, startedAt = None),
              fiber.cancel *> PlaybackExecution.sendAllNotesOff(send) *> logger.debug("Playback paused")
            )
          case None =>
            (state, IO.unit)
        }
      }.flatten
    }

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
      (
        PlaybackState(None, 0.millis, RepeatPolicy.none, None, None),
        cancel *> PlaybackExecution.sendAllSoundsOff(send) *> logger.debug("Playback stopped")
      )
    }.flatten

  override def play(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] =
    start(plan, policy, 0.millis)

  private def start(plan: PlaybackPlan, policy: RepeatPolicy, elapsed: FiniteDuration): IO[Unit] = {
    val adjustedPlan =
      if (elapsed <= zeroDuration) plan
      else PlaybackPlanResume.resumeFrom(plan, elapsed)

    for {
      ready <- Deferred[IO, Unit]
      now   <- IO.monotonic
      fiber <- (ready.get *> repeatLoop(adjustedPlan, plan, policy)).start
      _ <- stateRef.update(
        _.copy(
          activePlan = Some(plan),
          elapsed = elapsed,
          policy = policy,
          fiber = Some(fiber),
          startedAt = Some(now)
        )
      )
      _ <- ready.complete(())
    } yield ()
  }

  override def replace(plan: PlaybackPlan, policy: RepeatPolicy): IO[Unit] =
    IO.monotonic.flatMap { now =>
      stateRef.modify { state =>
        val elapsed = adjustedElapsed(elapsedAt(state, now), state.activePlan, plan)
        val cancel  = state.fiber.fold(IO.unit)(_.cancel)
        val initialChanges =
          if elapsed > zeroDuration then
            state.activePlan.fold(NonEmptyList.one(RestEvent))(PlaybackPlanDiff.changedInitialControlEvents(_, plan))
          else NonEmptyList.one(RestEvent)
        val sendInitialChanges = initialChanges.foldLeft(IO.unit)((action, event) => action *> send(event))
        val paused             = state.activePlan.nonEmpty && state.fiber.isEmpty
        val updated =
          state.copy(activePlan = Some(plan), elapsed = elapsed, policy = policy, fiber = None, startedAt = None)
        (
          updated,
          cancel *> PlaybackExecution.sendAllSoundsOff(send) *> sendInitialChanges *>
            (if paused then IO.unit else start(plan, policy, elapsed))
        )
      }.flatten
    }

  override def elapsedTime: IO[FiniteDuration] =
    IO.monotonic.flatMap(now => stateRef.get.map(elapsedAt(_, now)))

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
    policy: RepeatPolicy
  ): IO[Unit] = {
    def loop(
      currentPlan: PlaybackPlan,
      currentPolicy: RepeatPolicy
    ): IO[Unit] =
      PlaybackExecution
        .executeWithProgress(currentPlan, send)
        .flatMap { _ =>
          PlaybackController.nextRepeat(repeatedPlan, currentPolicy) match {
            case Some((nextPlan, nextPolicy)) =>
              IO.monotonic.flatMap(now =>
                stateRef.update(_.copy(elapsed = zeroDuration, startedAt = Some(now), policy = nextPolicy))
              ) *> loop(nextPlan, nextPolicy)
            case None =>
              stateRef.update(_.copy(elapsed = zeroDuration, fiber = None, activePlan = None, startedAt = None)).void
          }
        }

    loop(initialPlan, policy)
  }
}
