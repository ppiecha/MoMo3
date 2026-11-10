package app.domain

import cats.data.Ior
import cats.data.NonEmptyChain as NEC
import cats.data.Validated
import munit.FunSuite

class SectionSpec extends FunSuite {

  given Channel            = Channel.Channel0
  given Option[Instrument] = None

  test("Section.from accepts tracks with matching duration") {
    val first  = Track.track(Track.time(4), Track.note(60), duration = Some(Track.duration(4)))
    val second = Track.track(Track.time(4), Track.note(62), duration = Some(Track.duration(4)))

    Section.from(Validated.Valid(List(first, second))) match {
      case Ior.Right(tracks) =>
        assertEquals(tracks.toNonEmptyList.length, 2)
      case _ =>
        fail(s"Expected valid tracks but got errors")
    }
  }

  test("Section.from returns parse failure for empty input") {
    Section.from(Validated.Valid(List.empty)) match {
      case Ior.Left(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.NoValidTracksAndNoErrors)
        )
      case _ =>
        fail("Expected parse failure for empty tracks")
    }
  }

  test("Section.from maps parser errors to MusicFileParseFailed") {
    Section.from(Validated.Invalid(NEC.one("bad midi payload"))) match {
      case Ior.Left(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.MusicFileParseFailed("bad midi payload"))
        )
      case _ =>
        fail("Expected parse failure from invalid parsed tracks")
    }
  }

  test("Section.from rejects tracks with mismatched duration") {
    val reference = Track.track(Track.time(4), Track.note(60), duration = Some(Track.duration(4)))
    val mismatch  = Track.track(Track.time(2), Track.note(62), duration = Some(Track.duration(2)))

    val result = Section.from(Validated.Valid(List(reference, mismatch)))
    result match {
      case Ior.Both(errors, tracks) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.DurationMismatch(actual = 0.5, expected = 0.25, source = Some("SectionSpec.scala")))
        )
        assertEquals(
          DomainError.DurationMismatch(2.0, 4.0, Some("Music.scala")).toString,
          "Duration mismatch: source='Music.scala', actual=2.0, expected=4.0"
        )
        assertEquals(tracks.toNonEmptyList.length, 1) // Only the reference track is valid
        assertEquals(tracks.toNonEmptyList.head.timing.duration, 0.25)
      case _ =>
        fail("Expected duration mismatch validation errors")
    }
  }

  test("Section.from accumulates duration mismatches for multiple tracks") {
    val reference = Track.track(Track.time(4), Track.note(60), duration = Some(Track.duration(4)))
    val mismatch1 = Track.track(Track.time(2), Track.note(62), duration = Some(Track.duration(2)))
    val mismatch2 = Track.track(Track.time(1), Track.note(64), duration = Some(Track.duration(1)))

    Section.from(Validated.Valid(List(reference, mismatch1, mismatch2))) match {
      case Ior.Both(errors, tracks) =>
        assertEquals(
          errors.toChain.toList,
          List(
            DomainError.DurationMismatch(actual = 0.5, expected = 0.25, source = Some("SectionSpec.scala")),
            DomainError.DurationMismatch(actual = 1.0, expected = 0.25, source = Some("SectionSpec.scala"))
          )
        )
        assertEquals(tracks.toNonEmptyList.length, 1) // Only the reference track is valid
        assertEquals(tracks.toNonEmptyList.head.timing.duration, 0.25)
      case _ =>
        fail("Expected duration mismatch validation errors")
    }
  }

  test("Section.from returns Left when no valid tracks remain") {
    val negative = Track.track(Track.time(-1), Track.note(60), duration = Some(Track.duration(-1)))
    val second   = Track.track(Track.time(-1), Track.note(62), duration = Some(Track.duration(-1)))
    val third    = Track.track(Track.time(-1), Track.note(64), duration = Some(Track.duration(-1)))

    Section.from(Validated.Valid(List(negative, second, third))) match {
      case Ior.Left(errors) =>
        assertEquals(
          errors.toChain.toList,
          List.fill(3)(DomainError.NegativeDuration(-1.0))
        )
      case _ =>
        fail("Expected only validation errors and no valid tracks")
    }
  }
}
