package app.domain

export MidiValue.given

import cats.data.ValidatedNec
import cats.syntax.all.*

sealed trait NoteTag
sealed trait VelocityTag
object VelocityTag { val DEFAULT_VALUE = 100 }
sealed trait BankTag
sealed trait ProgramTag
sealed trait ControlTag

trait MidiValueKind[A] {
  def construct(value: Int): MidiValue[A]
}

sealed trait MidiValue[A] { def value: Int }

type Note     = MidiValue[NoteTag]
type Velocity = MidiValue[VelocityTag]
type Bank     = MidiValue[BankTag]
type Program  = MidiValue[ProgramTag]
type Control  = MidiValue[ControlTag]

object MidiValue {

  final case class Note private[MidiValue] (value: Int)     extends MidiValue[NoteTag]
  final case class Velocity private[MidiValue] (value: Int) extends MidiValue[VelocityTag]
  final case class Bank private[MidiValue] (value: Int)     extends MidiValue[BankTag]
  final case class Program private[MidiValue] (value: Int)  extends MidiValue[ProgramTag]
  final case class Control private[MidiValue] (value: Int)  extends MidiValue[ControlTag]

  given MidiValueKind[NoteTag] with     { def construct(value: Int): MidiValue[NoteTag] = Note(value)         }
  given MidiValueKind[VelocityTag] with { def construct(value: Int): MidiValue[VelocityTag] = Velocity(value) }
  given MidiValueKind[BankTag] with     { def construct(value: Int): MidiValue[BankTag] = Bank(value)         }
  given MidiValueKind[ProgramTag] with  { def construct(value: Int): MidiValue[ProgramTag] = Program(value)   }
  given MidiValueKind[ControlTag] with  { def construct(value: Int): MidiValue[ControlTag] = Control(value)   }

  val VELOCITY_ZERO: MidiValue[VelocityTag] = Velocity(0)

  def apply[A](value: Int)(using kind: MidiValueKind[A]): ValidatedNec[ValidationError, MidiValue[A]] =
    if value >= 0 && value <= 127 then kind.construct(value).validNec[ValidationError]
    else ValidationError.InvalidMidiValue(value).invalidNec[MidiValue[A]]

}
