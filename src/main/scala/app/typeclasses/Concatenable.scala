package app.typeclasses

import app.domain.DurationGen
import app.domain.Generator
import app.domain.Generator.NoteGen
import app.domain.Generator.VelocityGen
import app.domain.Note
import app.domain.NoteArg
import app.domain.TimeGen
import app.domain.Track
import app.domain.Velocity
import app.syntax.Extensions.repeat
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

  private def trackNotes(track: Track): Generator[Note] =
    track.notes.getOrElse(Generator.note(NoteArg.Single(0)).repeat(track.timing.length))

  private def trackDuration(track: Track): DurationGen =
    track.duration.getOrElse(DurationGen(track.timing.values))

  private def trackVelocity(track: Track): (NonEmptyList[Int], List[Int]) = {
    val values = track.velocity match {
      case Some(VelocityGen(values)) => values
      case None                      => NonEmptyList.one(Velocity.Default)
    }
    val (used, extra) = values.toList.splitAt(track.timing.length)
    (NonEmptyList.fromListUnsafe(used.padTo(track.timing.length, values.head)), extra)
  }

  given Concatenable[Track] with {
    def ++(left: Track, right: Track): Track = {
      require(left.channel == right.channel, "Cannot concatenate tracks with different channels")
      val notes = (left.notes, right.notes) match {
        case (None, None) => None
        case _            => Some(Concatenable[Generator[Note]].++(trackNotes(left), trackNotes(right)))
      }
      val duration = (left.duration, right.duration) match {
        case (None, None) => None
        case _            => Some(Concatenable[DurationGen].++(trackDuration(left), trackDuration(right)))
      }
      val velocity = (left.velocity, right.velocity) match {
        case (None, None) => None
        case _ =>
          val (leftValues, leftExtra)   = trackVelocity(left)
          val (rightValues, rightExtra) = trackVelocity(right)
          val combined                  = concatenate(leftValues, rightValues)
          Some(VelocityGen(NonEmptyList(combined.head, combined.tail ++ leftExtra ++ rightExtra)))
      }
      Track(
        timing = TimeGen(concatenate(left.timing.values, right.timing.values)),
        notes = notes,
        duration = duration,
        velocity = velocity,
        instrument = left.instrument.orElse(right.instrument),
        channel = left.channel,
        source = left.source.orElse(right.source),
        isActive = left.isActive
      )
    }
  }

}
