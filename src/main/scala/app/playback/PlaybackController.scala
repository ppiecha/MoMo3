package app.playback

import app.domain.AbsoluteMidiEvent
import app.domain.PlaybackPlan
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
    policy: RepeatPolicy = RepeatPolicy.none
  ): IO[Unit]
  def pause: IO[Unit]
  def resume: IO[Unit]
  def stop: IO[Unit]
  def replace(
    plan: PlaybackPlan,
    policy: RepeatPolicy = RepeatPolicy.none
  ): IO[Unit]
  def elapsedTime: IO[FiniteDuration]
}

object PlaybackController {
  def live(
    send: AbsoluteMidiEvent => IO[Unit],
    logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  ): IO[PlaybackController] =
    Ref.of[IO, PlaybackState](PlaybackState()).map(stateRef => new LivePlaybackController(send, logger, stateRef))

  private[playback] def nextRepeat(
    repeatedPlan: PlaybackPlan,
    policy: RepeatPolicy
  ): Option[(PlaybackPlan, RepeatPolicy)] =
    Option.when(policy.shouldRepeat)(repeatedPlan -> policy.next)
}

private final case class PlaybackState(
  activePlan: Option[PlaybackPlan] = None,
  elapsed: FiniteDuration = FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS),
  policy: RepeatPolicy = RepeatPolicy.none,
  fiber: Option[FiberIO[Unit]] = None
)

private final class LivePlaybackController(
  send: AbsoluteMidiEvent => IO[Unit],
  logger: Logger[IO],
  stateRef: Ref[IO, PlaybackState]
) extends PlaybackController {

  private val zeroDuration: FiniteDuration = FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS)

  override def play(plan: PlaybackPlan, policy: RepeatPolicy = RepeatPolicy.none): IO[Unit] =
    start(plan, policy, 0.millis)

  override def pause: IO[Unit] =
    stateRef.modify { state =>
      state.fiber match {
        case Some(fiber) =>
          (state.copy(fiber = None), fiber.cancel *> logger.info("Playback paused"))
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
      (PlaybackState(), cancel *> logger.info("Playback stopped"))
    }.flatten

  override def replace(plan: PlaybackPlan, policy: RepeatPolicy = RepeatPolicy.none): IO[Unit] =
    stateRef.modify { state =>
      val cancel = state.fiber.fold(IO.unit)(_.cancel)
      (state.copy(fiber = None), cancel *> start(plan, policy, state.elapsed))
    }.flatten

  override def elapsedTime: IO[FiniteDuration] =
    stateRef.get.map(_.elapsed)

//  private def buildPlan(tracks: Tracks, timing: TimingContext) =
//    IO.pure(PlaybackPlan.fromTracks(tracks, timing))

  private def start(
    plan: PlaybackPlan,
    policy: RepeatPolicy,
    elapsed: FiniteDuration
  ): IO[Unit] = {
    val adjustedPlan =
      if (elapsed <= zeroDuration) plan
      else PlaybackPlanResume.resumeFrom(plan, elapsed)

    val task: IO[Unit] = repeatLoop(adjustedPlan, plan, policy, elapsed)

    task.start.flatMap { fiber =>
      stateRef.update(_.copy(activePlan = Some(plan), elapsed = elapsed, policy = policy, fiber = Some(fiber)))
    }
  }

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
