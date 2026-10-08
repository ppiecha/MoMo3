package app.domain

import app.syntax.Extensions.repeat
import cats.data.{NonEmptyList, ValidatedNec}

enum Generator[A]:
  case NoteGen(steps: Seq[Chord]) extends Generator[Note]
  case VelocityGen(s: Seq[Int])   extends Generator[Velocity]

  def ++(other: Generator[A]): Generator[A] = (this, other) match
    case (NoteGen(s1), NoteGen(s2))         => NoteGen(s1 ++ s2)
    case (VelocityGen(s1), VelocityGen(s2)) => VelocityGen(s1 ++ s2)

  def length: Int = this match
    case NoteGen(s)     => s.length
    case VelocityGen(s) => s.length

  def repeat(n: Int): Generator[A] = this match
    case NoteGen(s)     => NoteGen(s.repeat(n))
    case VelocityGen(s) => VelocityGen(s.repeat(n))

object Generator {

  def parse[A](seq: Generator[A], ppq: Ppq): Seq[ValidatedNec[DomainError, A]] =
    seq match {
      case NoteGen(s)     => s.map(MidiValue[NoteTag])
      case VelocityGen(s) => s.map(MidiValue[VelocityTag])
    }

  def parseTicks(seq: TickGenerator, ppq: Ppq): Seq[ValidatedNec[DomainError, Tick]] =
    seq.values.map(d => Tick.fromDouble(d, ppq))

  def note(args: NoteArg*): Generator[Note] = {
    val chords: Seq[Chord] = args.map {
      case NoteArg.Single(n) => Chord(NonEmptyList.one(n))
      case NoteArg.Many(t) =>
        val ns = t.productIterator.toList.collect { case i: Int => i }
        ns match
          case h :: tail => Chord(NonEmptyList(h, tail))
          case Nil       => throw new IllegalArgumentException("Empty chord tuple")
    }
    Generator.NoteGen(chords)
  }

  def velocity(v: Int*): Generator[Velocity] = {
    VelocityGen(v)
  }

}
