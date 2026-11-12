package app.generic
import app.domain.Channel
import app.domain.DurationGen
import app.domain.Generator
import app.domain.Generator.NoteGen
import app.domain.Generator.VelocityGen
import app.domain.TimeGen
import app.domain.Track
import app.syntax.Extensions.++
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

  given Concatenable[TimeGen] with
    def ++(left: TimeGen, right: TimeGen): TimeGen =
      TimeGen(
        NonEmptyList(
          left.values.head,
          left.values.tail ++ right.values.toList
        )
      )

  given Concatenable[DurationGen] with
    def ++(left: DurationGen, right: DurationGen): DurationGen =
      DurationGen(
        NonEmptyList(
          left.values.head,
          left.values.tail ++ right.values.toList
        )
      )

  given [A](using concat: Concatenable[A]): Concatenable[Option[A]] with {
    def ++(left: Option[A], right: Option[A]): Option[A] =
      (left, right) match {
        case (Some(l), Some(r)) => Some(concat.++(l, r))
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
      }
  }

  given (using Channel): Concatenable[Track] with {
    def ++(left: Track, right: Track): Track =
      Track(
        timing = TimeGen(NonEmptyList(left.timing.values.head, left.timing.values.tail ++ right.timing.values.toList)),
        notes = (left.notes, right.notes) match {
          case (Some(n1), Some(n2)) => Some(n1 ++ n2)
          case (Some(n1), None)     => Some(n1)
          case (None, Some(n2))     => Some(n2)
          case (None, None)         => None
        },
        duration = (left.duration, right.duration) match {
          case (Some(d1), Some(d2)) =>
            Some(DurationGen(NonEmptyList(d1.values.head, d1.values.tail ++ d2.values.toList)))
          case (Some(d1), None) => Some(d1)
          case (None, Some(d2)) => Some(d2)
          case (None, None)     => None
        },
        velocity = (left.velocity, right.velocity) match {
          case (Some(v1), Some(v2)) => Some(v1 ++ v2)
          case (Some(v1), None)     => Some(v1)
          case (None, Some(v2))     => Some(v2)
          case (None, None)         => None
        },
        instrument = (left.instrument, right.instrument) match {
          case (Some(i1), Some(_)) => Some(i1)
          case (Some(i1), None)    => Some(i1)
          case (None, Some(i2))    => Some(i2)
          case (None, None)        => None
        },
        source = left.source.orElse(right.source),
        isActive = left.isActive
      )
  }

}
