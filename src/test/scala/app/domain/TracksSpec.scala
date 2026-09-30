package app.domain

import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import munit.FunSuite

class TracksSpec extends FunSuite {

  given Channel = Channel.Ch0

  test("Tracks.from accepts tracks with matching duration") {
    val first  = Track.track(Track.time(4), Track.duration(4), Track.note(60))
    val second = Track.track(Track.time(4), Track.duration(4), Track.note(62))

    Tracks.from(Seq(first, second)) match {
      case Valid(tracks) =>
        assertEquals(tracks.toSeq.size, 2)
      case Invalid(errors) =>
        fail(s"Expected valid tracks but got errors: ${errors.toChain.toList.mkString(", ")}")
    }
  }

  test("Tracks.from returns EmptyTracks for empty input") {
    Tracks.from(Seq.empty) match {
      case Valid(_) =>
        fail("Expected EmptyTracks validation error")
      case Invalid(errors) =>
        assertEquals(errors.toChain.toList, List(ValidationError.EmptyTracks))
    }
  }

  test("Tracks.from rejects tracks with mismatched duration") {
    val reference = Track.track(Track.time(4), Track.duration(4), Track.note(60))
    val mismatch  = Track.track(Track.time(2), Track.duration(2), Track.note(62))

    Tracks.from(Seq(reference, mismatch)) match {
      case Valid(_) =>
        fail("Expected duration mismatch validation error")
      case Invalid(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(ValidationError.DurationMismatch(actual = 2.0, expected = 4.0))
        )
    }
  }

  test("Tracks.from accumulates duration mismatches for multiple tracks") {
    val reference = Track.track(Track.time(4), Track.duration(4), Track.note(60))
    val mismatch1 = Track.track(Track.time(2), Track.duration(2), Track.note(62))
    val mismatch2 = Track.track(Track.time(1), Track.duration(1), Track.note(64))

    Tracks.from(Seq(reference, mismatch1, mismatch2)) match {
      case Valid(_) =>
        fail("Expected duration mismatch validation errors")
      case Invalid(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(
            ValidationError.DurationMismatch(actual = 2.0, expected = 4.0),
            ValidationError.DurationMismatch(actual = 1.0, expected = 4.0)
          )
        )
    }
  }
}
