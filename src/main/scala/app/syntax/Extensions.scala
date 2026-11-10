package app.syntax

import app.domain.Chord
import app.domain.Generator
import app.domain.Generator.NoteGen
import app.domain.Generator.VelocityGen
import app.generic.Concatenable
import cats.data.NonEmptyList

object Extensions {

  extension [A](a: A)(using concat: Concatenable[A]) {
    def ++(other: A): A = concat.++(a, other)
    def repeat(count: Int): A =
      LazyList.iterate(a)(concat.++(_, a)).take(count).lastOption.getOrElse(a)
  }

  extension [A](nel: NonEmptyList[A]) {
    def repeat(count: Int): NonEmptyList[A] =
      if count <= 1 then nel
      else NonEmptyList(nel.head, nel.tail ++ nel.repeat(count - 1).toList)
  }

  extension [A](opt: Option[Generator[A]]) {
    def transpose(semitones: Int): Option[Generator[A]] = {
      opt match
        case Some(gen) =>
          gen match
            case NoteGen(steps) =>
              Some(NoteGen(steps.map {
                case Chord.Voiced(notes) => Chord.Voiced(notes.map(n => if n == 0 then n else n + semitones))
                case Chord.Rest          => Chord.Rest
              }))
            case VelocityGen(_) => Some(gen)
        case None => None
    }
    def repeat(n: Int): Option[Generator[A]] = {
      opt match
        case Some(gen) => Some(gen.repeat(n))
        case None      => None
    }
  }

}
