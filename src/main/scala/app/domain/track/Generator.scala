package app.domain

import app.syntax.Extensions.repeat
import cats.data.{NonEmptyList, Validated, ValidatedNec}

sealed trait Generator[A] {

  import Generator.*

  def ++(other: Generator[A]): Generator[A] = (this, other) match
    case (NoteGen(s1), NoteGen(s2))         => NoteGen(NonEmptyList(s1.head, s1.tail ++ s2.toList))
    case (VelocityGen(s1), VelocityGen(s2)) => VelocityGen(NonEmptyList(s1.head, s1.tail ++ s2.toList))

  def length: Int = this match
    case NoteGen(s)     => s.length
    case VelocityGen(s) => s.length

  def repeat(n: Int): Generator[A] = this match
    case NoteGen(s)     => NoteGen(s.repeat(n))
    case VelocityGen(s) => VelocityGen(s.repeat(n))
}

object Generator {
  case class NoteGen(steps: NonEmptyList[Chord])    extends Generator[Note]
  case class VelocityGen(values: NonEmptyList[Int]) extends Generator[Velocity]

  def parseNotes(seq: Generator[Note]): NonEmptyList[ValidatedNec[DomainError, NonEmptyList[Note]]] =
    seq match
      case NoteGen(chords) =>
        chords.map {
          case Chord.Voiced(notes) => notes.traverse(Note.from)
          case Chord.Rest          => Validated.invalidNec(DomainError.EmptyChord)
        }

  def parseTicks(seq: TickGenerator)(using timingContext: TimingContext): NonEmptyList[ValidatedNec[DomainError, Tick]] =
    seq.values.map(d => Tick.from(d))

  def parseVelocity(seq: Generator[Velocity]): NonEmptyList[ValidatedNec[DomainError, Velocity]] =
    seq match
      case VelocityGen(values) => values.map(Velocity.from)

  def note(head: NoteArg, tail: NoteArg*): NoteGen = {
    val chords: NonEmptyList[Chord] = NonEmptyList(head, tail.toList).map {
      case NoteArg.Single(n) => Chord.Voiced(NonEmptyList.one(n))
      case NoteArg.Many(t) =>
        val ns = t.toList.collect { case i: Int => i }
        ns match
          case h :: tail => Chord.Voiced(NonEmptyList(h, tail))
          case Nil       => Chord.Rest
    }
    NoteGen(chords)
  }

  def velocity(head: Int, tail: Int*): VelocityGen = {
    VelocityGen(NonEmptyList(head, tail.toList))
  }

}
