package app.syntax

import app.domain.Generator.{NoteGen, VelocityGen}
import app.domain.{Chord, Generator}
import cats.data.NonEmptyList

object Extensions {

  extension [A](seq: Seq[A]) {
    def repeat(count: Int): Seq[A] = if count <= 0 then Seq.empty else seq ++ seq.repeat(count - 1)
  }

  extension [A](nel: NonEmptyList[A]) {
    def repeat(count: Int): NonEmptyList[A] =
      if count <= 1 then nel 
      else NonEmptyList(nel.head, nel.tail ++ nel.repeat(count - 1).toList)
  }
  
  extension [A](opt: Option[Generator[A]]) {
    def transpose(semitones: Int): Option[Generator[A]] = {
      opt match
        case Some(gen) => gen match
          case NoteGen(steps) => Some(NoteGen(steps.map {
            case Chord.Voiced(notes) => Chord.Voiced(notes.map(_ + semitones))
            case Chord.Rest    => Chord.Rest
          }))
          case VelocityGen(_) => Some(gen)
        case None => None
    }
    def ++(other: Option[Generator[A]]): Option[Generator[A]] = {
      (opt, other) match
        case (Some(gen1), Some(gen2)) => Some(gen1 ++ gen2)
        case (Some(gen1), None)       => Some(gen1)
        case (None, Some(gen2))       => Some(gen2)
        case (None, None)             => None
    }
    def repeat(n: Int): Option[Generator[A]] = {
      opt match
        case Some(gen) => Some(gen.repeat(n))
        case None      => None
    }
  }

}
