package app.generic
import app.domain.DurationGen
import app.domain.Generator
import app.domain.TimeGen
import app.domain.Track

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

  given concatenableOptionGenerator[A]: Concatenable[Option[Generator[A]]] with {
    def ++(left: Option[Generator[A]], right: Option[Generator[A]]): Option[Generator[A]] =
      (left, right) match {
        case (Some(l), Some(r)) => Some(l ++ r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
      }
  }

  given concatenableOptionTimeGen: Concatenable[Option[TimeGen]] with {
    def ++(left: Option[TimeGen], right: Option[TimeGen]): Option[TimeGen] =
      (left, right) match {
        case (Some(l), Some(r)) => Some(l ++ r)
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
