package app.generic
import app.domain.DurationGen
import app.domain.Generator
import app.domain.Generator.NoteGen
import app.domain.Generator.VelocityGen
import app.domain.Track
import cats.data.NonEmptyList

trait Concatenable[A] {
  def ++(left: A, right: A): A
}

object Concatenable {
  def apply[A](using c: Concatenable[A]): Concatenable[A] = c

  given [A]: Concatenable[Seq[A]] with {
    def ++(left: Seq[A], right: Seq[A]): Seq[A] = left ++ right
  }

  given Concatenable[String] with {
    def ++(left: String, right: String): String = left + right
  }

  given Concatenable[Track] with {
    def ++(left: Track, right: Track): Track = left ++ right
  }

  given [A]: Concatenable[Generator[A]] with {
    def ++(left: Generator[A], right: Generator[A]): Generator[A] = (left, right) match {
      case (NoteGen(s1), NoteGen(s2))         => NoteGen(NonEmptyList(s1.head, s1.tail ++ s2.toList))
      case (VelocityGen(s1), VelocityGen(s2)) => VelocityGen(NonEmptyList(s1.head, s1.tail ++ s2.toList))
    }
  }

  given Concatenable[NoteGen] with
    def ++(left: NoteGen, right: NoteGen): NoteGen =
      NoteGen(
        NonEmptyList(
          left.steps.head,
          left.steps.tail ++ right.steps.toList
        )
      )

  given Concatenable[VelocityGen] with
    def ++(left: VelocityGen, right: VelocityGen): VelocityGen =
      VelocityGen(
        NonEmptyList(
          left.values.head,
          left.values.tail ++ right.values.toList
        )
      )

  given concatOptGen[A](using concat: Concatenable[Generator[A]]): Concatenable[Option[Generator[A]]] with {
    def ++(left: Option[Generator[A]], right: Option[Generator[A]]): Option[Generator[A]] =
      (left, right) match {
        case (Some(l), Some(r)) => Some(concat.++(l, r))
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
      }
  }

  given [A](using concat: Concatenable[A]): Concatenable[Option[A]] with {
    def ++(left: Option[A], right: Option[A]): Option[A] =
      (left, right) match {
        case (Some(l), Some(r)) => Some(concat.++(l, r))
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
      }
  }

  given concatenableOptionDurationGen: Concatenable[Option[DurationGen]] with {
    def ++(left: Option[DurationGen], right: Option[DurationGen]): Option[DurationGen] =
      (left, right) match {
        case (Some(l), Some(r)) => Some(l ++ r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
      }
  }

}
