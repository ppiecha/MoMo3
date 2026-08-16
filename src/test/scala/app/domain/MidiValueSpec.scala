package app.domain

import munit.FunSuite
import cats.data.ValidatedNec
import cats.syntax.all.*
import app.domain.*

class MidiValueSpec extends FunSuite {

  test("MidiValue should create a valid Note") {
    val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](60)
    assert(result.isValid)
  }

  test("MidiValue should create an invalid Note for out of range value") {
    val result: ValidatedNec[ValidationError, MidiValue[NoteTag]] = MidiValue[NoteTag](128)
    assert(result.isInvalid)
  }

}
