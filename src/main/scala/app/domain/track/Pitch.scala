package app.domain

import app.domain.Generator.NoteGen
import cats.data.NonEmptyList

import scala.language.implicitConversions

/** A named MIDI pitch such as `c1`; `c1 * 15` repeats it as a note generator. */
final case class Pitch(value: Int) {
  def *(count: Int): NoteGen = {
    require(count >= 1, "Pitch repeat count must be at least 1")
    NoteGen(NonEmptyList.fromListUnsafe(List.fill(count)(Chord.Voiced(NonEmptyList.one(value)))))
  }
}

object Pitch {
  given Conversion[Pitch, Int] = _.value
}
