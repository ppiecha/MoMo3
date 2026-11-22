package app.syntax

import app.typeclasses.Concatenable
import app.typeclasses.Transposable

object Extensions {

  extension [A](a: A)(using concat: Concatenable[A]) {
    def ++(other: A): A = concat.++(a, other)
    def repeat(count: Int): A =
      LazyList.iterate(a)(concat.++(_, a)).take(count).lastOption.getOrElse(a)
  }

  extension [A](a: A)(using transposable: Transposable[A]) {
    def transpose(semitones: Int): A = transposable.transpose(a, semitones)
  }

}
