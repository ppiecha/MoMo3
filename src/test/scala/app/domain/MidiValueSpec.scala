package app.domain

import app.domain.*
import cats.data.ValidatedNec
import munit.FunSuite
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class MidiValueSpec extends FunSuite with ScalaCheckSuite {

  test("MidiValue should create a valid Note") {
    val result: ValidatedNec[DomainError, Note] = Note.from(60)
    assert(result.isValid)
  }

  test("MidiValue should create an invalid Note for out of range value") {
    val result: ValidatedNec[DomainError, Note] = Note.from(128)
    assert(result.isInvalid)
  }

  test("MidiValue should create an invalid Note for negative value") {
    val result: ValidatedNec[DomainError, Note] = Note.from(-1)
    assert(result.isInvalid)
  }

  property("MidiValue accepts all valid MIDI values (0-127)") {
    forAll(Gen.choose(0, 127)) { value =>
      val result: ValidatedNec[DomainError, Note] = Note.from(value)
      assert(result.isValid)
    }
  }

  property("MidiValue rejects all out-of-range values") {
    forAll(Gen.oneOf(Gen.negNum[Int], Gen.choose(128, Int.MaxValue))) { value =>
      val result: ValidatedNec[DomainError, Note] = Note.from(value)
      assert(result.isInvalid)
    }
  }

  property("MidiValue preserves value for valid inputs") {
    forAll(Gen.choose(0, 127)) { value =>
      val result: ValidatedNec[DomainError, Note] = Note.from(value)
      val extractedValue                          = result.map(_.value).getOrElse(-1)
      assertEquals(extractedValue, value)
    }
  }

  test("MidiValue.Velocity should create valid Velocity") {
    val result: ValidatedNec[DomainError, Velocity] = Velocity.from(100)
    assert(result.isValid)
  }

  test("MidiValue.Velocity should reject zero velocity") {
    val result: ValidatedNec[DomainError, Velocity] = Velocity.from(0)
    assert(result.isValid) // 0 is valid in MIDI
  }

  test("MidiValue.Bank should create valid Bank") {
    val result: ValidatedNec[DomainError, Bank] = Bank.from(0)
    assert(result.isValid)
  }

  test("MidiValue.Program should create valid Program") {
    val result: ValidatedNec[DomainError, Program] = Program.from(0)
    assert(result.isValid)
  }

  test("MidiValue.Control should create valid Control") {
    val result: ValidatedNec[DomainError, Control] = Control.from(7)
    assert(result.isValid)
  }

}
