package app.demo

import app.config.*
import cats.effect.*
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import Tracks.*
import app.domain.*
import app.playback.PlaybackRunner

object Main extends IOApp.Simple {

  def program: IO[Either[DomainError, Unit]] =
    Environment
      .from(bpm = 60)
      .toEither
      .leftMap(errors => DomainError.ValidationFailed(ValidationError.InvalidConfig(errors.toNonEmptyList)))
      .fold(
        err => IO.pure(Left(err)),
        env => PlaybackRunner.live(env).run(List(track4), env.timingContext)
      )

  def run: IO[Unit] = {
    val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    program.flatTap {
      case Left(err) => logger.error(s"Playback failed: $err")
      case Right(_)  => logger.info("Playback finished")
    }.void
  }

}
