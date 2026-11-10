package app.domain

import app.syntax.Extensions.repeat
import cats.data.Ior
import cats.data.IorNec
import cats.data.NonEmptyChain as NEC
import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.ValidatedNec

opaque type Section = NonEmptyList[Track]

object Section {

  def repeat(count: Int)(tracks: Section): Section = tracks.toNonEmptyList.repeat(count)

  def of(head: => Track, tail: => Track*): Section = NonEmptyList.of(head, tail*)

  def from(tracks: ValidatedNec[String, List[Track]]): IorNec[DomainError, Section] = tracks match
    case Validated.Invalid(errors) => Ior.Left(errors.map(DomainError.MusicFileParseFailed.apply))
    case Validated.Valid(tracks) =>
      tracks match
        case Nil          => Ior.Left(NEC.one(DomainError.NoValidTracksAndNoErrors))
        case head :: tail => validateTracksDuration(NonEmptyList(head, tail))

  private def validateTracksDuration(tracks: Section): IorNec[DomainError, Section] = {
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

  extension (tracks: Section) {
    def toNonEmptyList: NonEmptyList[Track] = tracks
    def toList: List[Track]                 = tracks.toList
  }
}
