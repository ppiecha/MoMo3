package app.domain

import cats.data.Ior
import cats.data.IorNec
import cats.data.Validated
import cats.data.ValidatedNec
import cats.data.{NonEmptyChain => NEC}

opaque type Tracks = Seq[Track]

object Tracks {
  def from(tracks: ValidatedNec[String, Seq[Track]]): IorNec[DomainError, Tracks] = tracks match
    case Validated.Invalid(errors) => Ior.Left(errors.map(DomainError.MusicFileParseFailed.apply))
    case Validated.Valid(tracks) =>
      tracks match
        case Nil => Ior.Left(NEC.one(DomainError.MusicFileParseFailed("Music file returned no tracks")))
        case nonEmptyTracks => validateTrackDurations(nonEmptyTracks)

  private def validateTrackDurations(tracks: Seq[Track]): IorNec[DomainError, Tracks] = {
    val expectedDuration = tracks.head.timeGen.duration
    val validated        = tracks.map(_.compareDuration(expectedDuration))

    val errors = validated.flatMap {
      case Ior.Left(errs)      => errs.toChain.toList
      case Ior.Both(errs, _)   => errs.toChain.toList
      case Ior.Right(_)        => Nil
    }

    val validTracks = validated.flatMap {
      case Ior.Right(track)    => List(track)
      case Ior.Both(_, track)  => List(track)
      case Ior.Left(_)         => Nil
    }

    NEC.fromSeq(errors) match
      case Some(necErrors) if validTracks.isEmpty => Ior.Left(necErrors)
      case Some(necErrors)                        => Ior.Both(necErrors, validTracks)
      case None                                   => Ior.Right(validTracks)
  }

  extension (tracks: Tracks) {
    def toSeq: Seq[Track] = tracks
  }
}
