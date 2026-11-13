package app.generic
import app.domain.Channel
import app.domain.DurationGen
import app.domain.Generator
import app.domain.Generator.NoteGen
import app.domain.Generator.VelocityGen
import app.domain.Note
import app.domain.TimeGen
import app.domain.Track
import app.domain.Velocity
import cats.data.NonEmptyList

trait Concatenable[A] {
  def ++(left: A, right: A): A
}

object Concatenable {
  def apply[A](using c: Concatenable[A]): Concatenable[A] = c

  private def concatenate[A](left: NonEmptyList[A], right: NonEmptyList[A]): NonEmptyList[A] =
    NonEmptyList(left.head, left.tail ++ right.toList)

  given [A]: Concatenable[Seq[A]] with {
    def ++(left: Seq[A], right: Seq[A]): Seq[A] = left ++ right
  }

  given Concatenable[String] with {
    def ++(left: String, right: String): String = left + right
  }

  given [A]: Concatenable[Generator[A]] with {
    def ++(left: Generator[A], right: Generator[A]): Generator[A] = (left, right) match {
      case (NoteGen(s1), NoteGen(s2))         => NoteGen(concatenate(s1, s2))
      case (VelocityGen(s1), VelocityGen(s2)) => VelocityGen(concatenate(s1, s2))
    }
  }

  given Concatenable[NoteGen] with
    def ++(left: NoteGen, right: NoteGen): NoteGen =
      NoteGen(concatenate(left.steps, right.steps))

  given Concatenable[VelocityGen] with
    def ++(left: VelocityGen, right: VelocityGen): VelocityGen =
      VelocityGen(concatenate(left.values, right.values))

  given Concatenable[TimeGen] with
    def ++(left: TimeGen, right: TimeGen): TimeGen =
      TimeGen(concatenate(left.values, right.values))

  given Concatenable[DurationGen] with
    def ++(left: DurationGen, right: DurationGen): DurationGen =
      DurationGen(concatenate(left.values, right.values))

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
        timing = TimeGen(concatenate(left.timing.values, right.timing.values)),
        notes = Concatenable[Option[Generator[Note]]].++(left.notes, right.notes),
        duration = Concatenable[Option[DurationGen]].++(left.duration, right.duration),
        velocity = Concatenable[Option[Generator[Velocity]]].++(left.velocity, right.velocity),
        instrument = left.instrument.orElse(right.instrument),
        source = left.source.orElse(right.source),
        isActive = left.isActive
      )
  }

}
