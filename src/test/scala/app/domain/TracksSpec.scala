package app.domain

import cats.data.Ior
import munit.FunSuite

class TracksSpec extends FunSuite {

  given Channel = Channel.Ch0

  test("Tracks.from accepts tracks with matching duration") {
    val first  = Track.track(Track.time(4), Track.duration(4), Track.note(60))
    val second = Track.track(Track.time(4), Track.duration(4), Track.note(62))

    Tracks.from(Seq(first, second)) match {
      case Ior.Right(tracks) =>
        assertEquals(tracks.toSeq.size, 2)
      case _ =>
        fail(s"Expected valid tracks but got errors")
    }
  }

  test("Tracks.from returns EmptyTracks for empty input") {
    Tracks.from(Seq.empty) match {
      case Ior.Left(errors) =>
        assertEquals(errors.toChain.toList, List(DomainError.EmptyTracks))
      case _ =>
        fail("Expected EmptyTracks validation error")
    }
  }

  test("Tracks.from rejects tracks with mismatched duration") {
    val reference = Track.track(Track.time(4), Track.duration(4), Track.note(60))
    val mismatch  = Track.track(Track.time(2), Track.duration(2), Track.note(62))

    val result = Tracks.from(Seq(reference, mismatch))
    result match {
      case Ior.Both(errors, tracks) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.DurationMismatch(actual = 2.0, expected = 4.0))
        )
        assertEquals(tracks.toSeq.size, 1) // Only the reference track is valid
        assertEquals(tracks.toSeq.head.timeGen.duration, 4.0)
      case _ =>
        fail("Expected duration mismatch validation errors")
    }
  }

  test("Tracks.from accumulates duration mismatches for multiple tracks") {
    val reference = Track.track(Track.time(4), Track.duration(4), Track.note(60))
    val mismatch1 = Track.track(Track.time(2), Track.duration(2), Track.note(62))
    val mismatch2 = Track.track(Track.time(1), Track.duration(1), Track.note(64))

    Tracks.from(Seq(reference, mismatch1, mismatch2)) match {
      case Ior.Both(errors, tracks) =>
        assertEquals(
          errors.toChain.toList,
          List(
            DomainError.DurationMismatch(actual = 2.0, expected = 4.0),
            DomainError.DurationMismatch(actual = 1.0, expected = 4.0)
          )
        )
        assertEquals(tracks.toSeq.size, 1) // Only the reference track is valid
        assertEquals(tracks.toSeq.head.timeGen.duration, 4.0)
      case _ =>
        fail("Expected duration mismatch validation errors")
    }
  }

  test("Tracks.from returns Left when no valid tracks remain") {
    val negative = Track.track(Track.time(-1), Track.duration(-1), Track.note(60))
    val second   = Track.track(Track.time(2), Track.duration(2), Track.note(62))
    val third    = Track.track(Track.time(1), Track.duration(1), Track.note(64))

    Tracks.from(Seq(negative, second, third)) match {
      case Ior.Left(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(
            DomainError.NegativeDuration(-1.0),
            DomainError.NegativeDuration(-1.0),
            DomainError.NegativeDuration(-1.0)
          )
        )
      case _ =>
        fail("Expected only validation errors and no valid tracks")
    }
  }
}
