package app.tracks

import app.config.Environment
import app.domain.*
import app.playback.{PlaybackPipeline, PlaybackService}
import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import java.nio.file.{Files => JFiles, Path => JPath}
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

object TrackFolderSupervisor {

  def run[F[_]: Async: Logger](
    folder: JPath,
    env: Environment,
    send: AbsoluteMidiEvent => F[Unit]
  ): F[Unit] =
    for
      _        <- Async[F].blocking(JFiles.createDirectories(folder))
      initial  <- loadTracks[F](folder)
      _        <- Logger[F].info(s"Loaded ${initial.size} track(s) from $folder")
      trackRef <- Ref.of[F, List[Track]](initial)
      _        <- (pollFolder[F](folder, trackRef), loopPlay[F](trackRef, env, send)).parTupled
    yield ()

  private def loadTracks[F[_]: Async: Logger](folder: JPath): F[List[Track]] =
    Async[F]
      .blocking {
        val stream = JFiles.newDirectoryStream(folder, "*.track")
        try stream.iterator().asScala.toList
        finally stream.close()
      }
      .flatMap { paths =>
        paths.traverse { path =>
          Async[F].blocking(JFiles.readString(path)).flatMap { content =>
            TrackFileParser.parse(content) match
              case Right(track) => track.some.pure[F]
              case Left(err)    => Logger[F].warn(s"Failed to parse $path: $err").as(none[Track])
          }
        }
      }
      .map(_.flatten)

  private def pollFolder[F[_]: Async: Logger](
    folder: JPath,
    ref: Ref[F, List[Track]]
  ): F[Unit] =
    Stream
      .awakeEvery[F](2.seconds)
      .evalMap { _ =>
        loadTracks[F](folder).flatMap { tracks =>
          Logger[F].debug(s"Reloaded ${tracks.size} track(s)") *> ref.set(tracks)
        }
      }
      .compile
      .drain

  private def loopPlay[F[_]: Async: Logger](
    ref: Ref[F, List[Track]],
    env: Environment,
    send: AbsoluteMidiEvent => F[Unit]
  ): F[Unit] =
    Stream
      .repeatEval {
        ref.get.flatMap { tracks =>
          if tracks.isEmpty then
            Logger[F].debug("No tracks loaded, waiting...") *> Temporal[F].sleep(1.second)
          else
            PlaybackPipeline.live.build(tracks, env.timingContext) match
              case Left(err) =>
                Logger[F].error(s"Failed to build playback plan: $err") *> Temporal[F].sleep(1.second)
              case Right(plan) =>
                PlaybackService.executePlaybackPlan(plan, send)
        }
      }
      .compile
      .drain
}
