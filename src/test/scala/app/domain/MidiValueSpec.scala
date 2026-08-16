package app.domain

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import cats.data.ValidatedNec
import app.domain.*

class MidiValueSpec extends FunSuite with ScalaCheckSuite {

  test("MidiValue should create a valid Note") {
    val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](60)
    assert(result.isValid)
  }

  test("MidiValue should create an invalid Note for out of range value") {
    val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](128)
    assert(result.isInvalid)
  }

  test("MidiValue should create an invalid Note for negative value") {
    val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](-1)
    assert(result.isInvalid)
  }

  property("MidiValue accepts all valid MIDI values (0-127)") {
    forAll(Gen.choose(0, 127)) { value =>
      val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](value)
      assert(result.isValid)
    }
  }

  property("MidiValue rejects all out-of-range values") {
    forAll(Gen.oneOf(Gen.negNum[Int], Gen.choose(128, Int.MaxValue))) { value =>
      val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](value)
      assert(result.isInvalid)
    }
  }

  property("MidiValue preserves value for valid inputs") {
    forAll(Gen.choose(0, 127)) { value =>
      val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](value)
      val extractedValue                                            = result.map(_.value).getOrElse(-1)
      assertEquals(extractedValue, value)
    }
  }

  test("MidiValue.Velocity should create valid Velocity") {
    val result: ValidatedNec[ValidationError, MidiValue[VelocityTag]] = MidiValue[VelocityTag](100)
    assert(result.isValid)
  }

  test("MidiValue.Velocity should reject zero velocity") {
    val result: ValidatedNec[ValidationError, MidiValue[VelocityTag]] = MidiValue[VelocityTag](0)
    assert(result.isValid) // 0 is valid in MIDI
  }

  test("MidiValue.Bank should create valid Bank") {
    val result: ValidatedNec[ValidationError, MidiValue[BankTag]] = MidiValue[BankTag](0)
    assert(result.isValid)
  }

  test("MidiValue.Program should create valid Program") {
    val result: ValidatedNec[ValidationError, MidiValue[ProgramTag]] = MidiValue[ProgramTag](0)
    assert(result.isValid)
  }

  test("MidiValue.Control should create valid Control") {
    val result: ValidatedNec[ValidationError, MidiValue[ControlTag]] = MidiValue[ControlTag](7)
    assert(result.isValid)
  }

}
