package app.syntax
import app.domain.Section
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec

final case class MusicDefinition(section: Section, bpm: Option[Int])

trait MusicFile {
  def bpm: Option[Int] = None

  def musicWrapper: ValidatedNec[String, MusicDefinition] =
    playWrapper.andThen { section =>
      Validated
        .catchNonFatal(MusicDefinition(section, bpm))
        .leftMap(e => NonEmptyChain.one(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }

  def playWrapper: ValidatedNec[String, Section] =
    Validated
      .catchNonFatal(play)
      .leftMap { e =>
        NonEmptyChain.one(
          s"${e.getClass.getSimpleName}: ${e.getMessage}"
        )
      }

  def play: Section
}
