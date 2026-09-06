package app.playback

import app.domain.{AbsoluteMidiEvent, DomainError, PlaybackPlan, TimingContext, Track}
import cats.effect.{FiberIO, IO, Ref}
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** High-level playback control for start, pause, resume, stop and replacing the current plan with a freshly compiled
  * set of tracks.
  */
trait PlaybackController {
  def play(
    tracks: List[Track],
    timing: TimingContext,
    policy: RepeatPolicy = RepeatPolicy.none
  ): IO[Unit]
  def pause: IO[Unit]
  def resume: IO[Unit]
  def stop: IO[Unit]
  def replace(tracks: List[Track], timing: TimingContext, policy: RepeatPolicy = RepeatPolicy.none): IO[Unit]
  def elapsedTime: IO[FiniteDuration]
}

object PlaybackController {
  def live(
    send: AbsoluteMidiEvent => IO[Unit],
    logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  ): PlaybackController =
    new LivePlaybackController(send, logger)

  private[playback] def nextRepeat(
    repeatedPlan: PlaybackPlan,
    policy: RepeatPolicy
  ): Option[(PlaybackPlan, RepeatPolicy)] =
    policy match {
      case RepeatPolicy.None                       => None
      case RepeatPolicy.Fixed(count) if count <= 0 => None
      case RepeatPolicy.Fixed(count)               => Some(repeatedPlan -> RepeatPolicy.Fixed(count - 1))
      case RepeatPolicy.Forever                    => Some(repeatedPlan -> RepeatPolicy.Forever)
    }
}

private final case class PlaybackState(
  activePlan: Option[PlaybackPlan] = None,
  elapsed: FiniteDuration = FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS),
  policy: RepeatPolicy = RepeatPolicy.none,
  fiber: Option[FiberIO[Unit]] = None
)

private final class LivePlaybackController(
  send: AbsoluteMidiEvent => IO[Unit],
  logger: Logger[IO]
) extends PlaybackController {

  private val stateRef: Ref[IO, PlaybackState] = Ref.unsafe(PlaybackState())

  override def play(
    tracks: List[Track],
    timing: TimingContext,
    policy: RepeatPolicy = RepeatPolicy.none
  ): IO[Unit] =
    buildPlan(tracks, timing).flatMap {
      case Left(err) =>
        logger.error(s"Playback failed: $err") *> IO.unit
      case Right(plan) =>
        start(plan, policy, 0.millis)
    }

  override def pause: IO[Unit] =
    stateRef.get.flatMap { state =>
      state.fiber match {
        case Some(fiber) =>
          fiber.cancel *> stateRef.update(_.copy(fiber = None)).void *> logger.info("Playback paused")
        case None =>
          IO.unit
      }
    }

  override def resume: IO[Unit] =
    stateRef.get.flatMap { state =>
      state.activePlan match {
        case Some(plan) => start(plan, state.policy, state.elapsed)
        case None       => IO.unit
      }
    }

  override def stop: IO[Unit] =
    stateRef.get.flatMap { state =>
      state.fiber match {
        case Some(fiber) => fiber.cancel
        case None        => IO.unit
      }
    } *> stateRef.set(PlaybackState()) *> logger.info("Playback stopped")

  override def replace(
    tracks: List[Track],
    timing: TimingContext,
    policy: RepeatPolicy = RepeatPolicy.none
  ): IO[Unit] =
    stateRef.get.flatMap { state =>
      buildPlan(tracks, timing).flatMap {
        case Left(err) =>
          logger.error(s"Playback replacement failed: $err") *> IO.unit
        case Right(plan) =>
          (
            state.fiber match {
              case Some(fiber) => fiber.cancel *> stateRef.update(_.copy(fiber = None))
              case None        => IO.unit
            }
          ) *> start(plan, policy, state.elapsed)
      }
    }

  override def elapsedTime: IO[FiniteDuration] =
    stateRef.get.map(_.elapsed)

  private def buildPlan(tracks: List[Track], timing: TimingContext): IO[Either[DomainError, PlaybackPlan]] =
    IO.pure(PlaybackPipeline.live.build(tracks, timing))

  private def start(
    plan: PlaybackPlan,
    policy: RepeatPolicy,
    elapsed: FiniteDuration
  ): IO[Unit] = {
    val adjustedPlan =
      if (elapsed <= FiniteDuration(0L, scala.concurrent.duration.MILLISECONDS)) plan
      else PlaybackPlanResume.resumeFrom(plan, elapsed)

    val task: IO[Unit] = repeatLoop(adjustedPlan, plan, policy, elapsed)

    // logger.info(s"plan: $plan \nadjustedPlan: $adjustedPlan") *>
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
