package app.domain

import cats.data.{Ior, IorNec, NonEmptyList, Validated, ValidatedNec, NonEmptyChain as NEC}

opaque type Tracks = NonEmptyList[Track]

object Tracks {
  def from(tracks: ValidatedNec[String, List[Track]]): IorNec[DomainError, Tracks] = tracks match
    case Validated.Invalid(errors) => Ior.Left(errors.map(DomainError.MusicFileParseFailed.apply))
    case Validated.Valid(tracks)   => tracks match
      case Nil          => Ior.Left(NEC.one(DomainError.NoValidTracksAndNoErrors))
      case head :: tail => validateTrackDurations(NonEmptyList(head, tail))


  private def validateTrackDurations(tracks: NonEmptyList[Track]): IorNec[DomainError, Tracks] = {
    val expectedDuration = tracks.head.timing.duration
    val validated        = tracks.map(_.compareDuration(expectedDuration))

    val errors = validated.toList.flatMap {
      case Ior.Left(errs)    => errs.toNonEmptyList.toList
      case Ior.Both(errs, _) => errs.toChain.toList.toList
      case Ior.Right(_)      => Nil
    }

    val validTracks = validated.toList.flatMap {
      case Ior.Right(track)   => List(track)
      case Ior.Both(_, track) => List(track)
      case Ior.Left(_)        => Nil
    }

    NEC.fromSeq(errors) match
      case Some(necErrors) =>
        validTracks match {
          case Nil          => Ior.Left(necErrors)
          case head :: tail => Ior.Both(necErrors, NonEmptyList(head, tail))
        }
      case None =>
        validTracks match {
          case Nil          => Ior.Left(NEC.one(DomainError.NoValidTracksAndNoErrors))
          case head :: tail => Ior.Right(NonEmptyList(head, tail))
        }
  }

  extension (tracks: Tracks) {
    def toSeq: NonEmptyList[Track] = tracks
  }
}
