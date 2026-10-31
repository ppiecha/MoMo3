package app.domain

export MidiValue.given

import cats.data.ValidatedNec
import cats.syntax.all.*

sealed trait NoteTag
sealed trait VelocityTag
object VelocityTag
sealed trait BankTag
sealed trait ProgramTag
sealed trait ControlTag

trait MidiValueKind[A] {
  def maxValue: Int = 127
  def construct(value: Int): MidiValue[A]
}

sealed trait MidiValue[A] { def value: Int }

type Note     = MidiValue[NoteTag]
type Velocity = MidiValue[VelocityTag]
type Bank     = MidiValue[BankTag]
type Program  = MidiValue[ProgramTag]
type Control  = MidiValue[ControlTag]

object Note {
  final val Zero = 0
  def from(value: Int): ValidatedNec[DomainError, Note] =
    MidiValue[NoteTag](value)
}

object Velocity {
  final val Default = 120
  final val Zero    = 0
  def from(value: Int): ValidatedNec[DomainError, Velocity] =
    MidiValue[VelocityTag](value)
}

object Bank {
  val Default: Int    = 0
  val Percussion: Int = 128
  def from(value: Int): ValidatedNec[DomainError, Bank] =
    MidiValue[BankTag](value)
}

object Program {
  val Default = 0
  def from(value: Int): ValidatedNec[DomainError, Program] =
    MidiValue[ProgramTag](value)
}

object Control {
  def from(value: Int): ValidatedNec[DomainError, Control] =
    MidiValue[ControlTag](value)
}

object MidiValue {

  final case class Note private[MidiValue] (value: Int)     extends MidiValue[NoteTag]
  final case class Velocity private[MidiValue] (value: Int) extends MidiValue[VelocityTag]
  final case class Bank private[MidiValue] (value: Int)     extends MidiValue[BankTag]
  final case class Program private[MidiValue] (value: Int)  extends MidiValue[ProgramTag]
  final case class Control private[MidiValue] (value: Int)  extends MidiValue[ControlTag]

  given MidiValueKind[NoteTag] with     { def construct(value: Int): MidiValue[NoteTag] = Note(value)         }
  given MidiValueKind[VelocityTag] with { def construct(value: Int): MidiValue[VelocityTag] = Velocity(value) }
  given MidiValueKind[BankTag] with {
    override def maxValue: Int                    = 16383 // 14-bit Bank Select (MSB/LSB)
    def construct(value: Int): MidiValue[BankTag] = Bank(value)
  }
  given MidiValueKind[ProgramTag] with { def construct(value: Int): MidiValue[ProgramTag] = Program(value) }
  given MidiValueKind[ControlTag] with { def construct(value: Int): MidiValue[ControlTag] = Control(value) }

  val VelocityZero: MidiValue[VelocityTag] = Velocity(0)

  def apply[A](value: Int)(using kind: MidiValueKind[A]): ValidatedNec[DomainError, MidiValue[A]] =
    if value >= 0 && value <= kind.maxValue then kind.construct(value).validNec[DomainError]
    else DomainError.InvalidMidiValue(value).invalidNec[MidiValue[A]]

}
