package app.domain
import cats.data.NonEmptyList

sealed trait Chord

object Chord {
  final case class Voiced(notes: NonEmptyList[Int]) extends Chord
  case object Rest                                  extends Chord
}
