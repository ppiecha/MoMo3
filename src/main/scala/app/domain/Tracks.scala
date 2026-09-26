package app.domain

import cats.data.{Validated, ValidatedNec}
import cats.syntax.all.*

opaque type Tracks = Seq[Track]

object Tracks {
  def from(tracks: Seq[Track]): ValidatedNec[ValidationError, Tracks] = tracks match
    case Nil => ValidationError.EmptyTracks.invalidNec
    case tracks =>
      tracks.traverse(_.validatedDuration(tracks.head.timeGen.duration)) match
        case Validated.Valid(tracks) => tracks.validNec[ValidationError]
        case Validated.Invalid(errors) => errors.invalid[Tracks]

  extension (tracks: Tracks) {
    def toSeq: Seq[Track] = tracks
  }
}