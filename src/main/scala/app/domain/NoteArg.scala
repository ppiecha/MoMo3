package app.domain

import cats.data.NonEmptyList
import scala.language.implicitConversions

final case class Chord(notes: NonEmptyList[Int])

sealed trait NoteArg
object NoteArg {
  final case class Single(n: Int) extends NoteArg
  final case class Many(t: Tuple) extends NoteArg

  given Conversion[Int, NoteArg] = Single(_)
  given Conversion[Tuple, NoteArg] = Many(_)
}

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
