package app.demo

import app.config.Environment
import app.domain.{DomainError, TimingContext}
import app.playback.{PlaybackController, RepeatPolicy, TrackDirectoryMonitor, TrackFileCompiler}
import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.Paths

/** Example integration for watching a folder with track files.
  *
  * The monitor scans the directory for .scala files, validates them, compiles the valid ones, and rebuilds the playback
  * plan using the current timing context.
  */
object TrackDirectoryDemo {

  def runMonitoring(directory: String, repeat: RepeatPolicy = RepeatPolicy.none): IO[Unit] = {
    val logger: Logger[IO] = Slf4jLogger.getLogger[IO]

    Environment
      .from(bpm = 120)
      .fold(
        err => logger.error(s"Invalid environment: $err") *> IO.unit,
        env => {
          val send       = (event: app.domain.AbsoluteMidiEvent) => IO.unit
          val controller = PlaybackController.live(send, logger)
          val monitor = TrackDirectoryMonitor.live(
            directory = Paths.get(directory),
            parser = TrackFileCompiler.compileAndEvaluateFile,
            compiler = track => app.playback.TrackCompiler.compile(track, env.timingContext),
            playback = controller,
            timing = env.timingContext,
            policy = repeat,
            logger = logger
          )

          logger.info(s"Watching track directory: $directory") *>
            monitor.start *>
            IO.never
        }
      )
  }
}
