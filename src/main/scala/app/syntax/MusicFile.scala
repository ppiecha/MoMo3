package app.syntax
import app.domain.Track
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec

trait MusicFile {
  def playWrapper: ValidatedNec[String, Seq[Track]] =
    Validated
      .catchNonFatal(play)
      .leftMap { e =>
        NonEmptyChain.one(
          s"${e.getClass.getSimpleName}: ${e.getMessage}"
        )
      }

  def play: Seq[Track]
}
