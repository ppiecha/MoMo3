package app.syntax
import app.domain.Tracks
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec

trait MusicFile {
  def playWrapper: ValidatedNec[String, Tracks] =
    Validated
      .catchNonFatal(play)
      .leftMap { e =>
        NonEmptyChain.one(
          s"${e.getClass.getSimpleName}: ${e.getMessage}"
        )
      }

  def play: Tracks
}
