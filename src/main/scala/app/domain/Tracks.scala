package app.domain

import cats.data.Ior
import cats.data.IorNec
import cats.data.{NonEmptyChain => NEC}

opaque type Tracks = Seq[Track]

object Tracks {
  def from(tracks: Seq[Track]): IorNec[DomainError, Tracks] = tracks match
    case Nil => Ior.Left(NEC.one(DomainError.EmptyTracks))
    case tracks =>
      val expectedDuration = tracks.head.timeGen.duration
      val (errors, validTracks) = tracks.foldLeft((List.empty[DomainError], Vector.empty[Track])) {
        case ((accErrors, accTracks), track) =>
          track.compareDuration(expectedDuration) match
            case Ior.Left(errs)         => (accErrors ++ errs.toChain.toList, accTracks)
            case Ior.Right(validTrack)  => (accErrors, accTracks :+ validTrack)
            case Ior.Both(errs, result) => (accErrors ++ errs.toChain.toList, accTracks :+ result)
      }

      NEC.fromSeq(errors) match
        case Some(necErrors) if validTracks.isEmpty => Ior.Left(necErrors)
        case Some(necErrors)                        => Ior.Both(necErrors, validTracks)
        case None                                   => Ior.Right(validTracks)

  extension (tracks: Tracks) {
    def toSeq: Seq[Track] = tracks
  }
}
