package app.syntax
import app.domain.Track
import cats.data.{NonEmptyChain, Validated, ValidatedNec}

trait TrackFile {
  def playWrapper: ValidatedNec[String, Track] =
    Validated
      .catchNonFatal(apply())
      .leftMap { e =>
        NonEmptyChain.one(
          s"Track ${this.getClass.getName.stripSuffix("$")} exception: ${e.getClass.getSimpleName}: ${e.getMessage}"
        )
      }

  def apply(): Track
}


