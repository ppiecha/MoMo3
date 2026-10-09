package app.domain

import scala.language.implicitConversions

sealed trait NoteArg
object NoteArg {
  final case class Single(n: Int) extends NoteArg
  final case class Many(t: Tuple) extends NoteArg

  given Conversion[Int, NoteArg] = Single(_)
  given Conversion[Tuple, NoteArg] = Many(_)
}
