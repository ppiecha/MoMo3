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
  final val r: Pitch   = Pitch(0)
  final val c0: Pitch  = Pitch(12)
  final val cs0: Pitch = Pitch(13)
  final val d0: Pitch  = Pitch(14)
  final val ds0: Pitch = Pitch(15)
  final val e0: Pitch  = Pitch(16)
  final val f0: Pitch  = Pitch(17)
  final val fs0: Pitch = Pitch(18)
  final val g0: Pitch  = Pitch(19)
  final val gs0: Pitch = Pitch(20)
  final val a0: Pitch  = Pitch(21)
  final val as0: Pitch = Pitch(22)
  final val b0: Pitch  = Pitch(23)
  final val c1: Pitch  = Pitch(24)
  final val cs1: Pitch = Pitch(25)
  final val d1: Pitch  = Pitch(26)
  final val ds1: Pitch = Pitch(27)
  final val e1: Pitch  = Pitch(28)
  final val f1: Pitch  = Pitch(29)
  final val fs1: Pitch = Pitch(30)
  final val g1: Pitch  = Pitch(31)
  final val gs1: Pitch = Pitch(32)
  final val a1: Pitch  = Pitch(33)
  final val as1: Pitch = Pitch(34)
  final val b1: Pitch  = Pitch(35)
  final val c2: Pitch  = Pitch(36)
  final val cs2: Pitch = Pitch(37)
  final val d2: Pitch  = Pitch(38)
  final val ds2: Pitch = Pitch(39)
  final val e2: Pitch  = Pitch(40)
  final val f2: Pitch  = Pitch(41)
  final val fs2: Pitch = Pitch(42)
  final val g2: Pitch  = Pitch(43)
  final val gs2: Pitch = Pitch(44)
  final val a2: Pitch  = Pitch(45)
  final val as2: Pitch = Pitch(46)
  final val b2: Pitch  = Pitch(47)
  final val c3: Pitch  = Pitch(48)
  final val cs3: Pitch = Pitch(49)
  final val d3: Pitch  = Pitch(50)
  final val ds3: Pitch = Pitch(51)
  final val e3: Pitch  = Pitch(52)
  final val f3: Pitch  = Pitch(53)
  final val fs3: Pitch = Pitch(54)
  final val g3: Pitch  = Pitch(55)
  final val gs3: Pitch = Pitch(56)
  final val a3: Pitch  = Pitch(57)
  final val as3: Pitch = Pitch(58)
  final val b3: Pitch  = Pitch(59)
  final val c4: Pitch  = Pitch(60)
  final val cs4: Pitch = Pitch(61)
  final val d4: Pitch  = Pitch(62)
  final val ds4: Pitch = Pitch(63)
  final val e4: Pitch  = Pitch(64)
  final val f4: Pitch  = Pitch(65)
  final val fs4: Pitch = Pitch(66)
  final val g4: Pitch  = Pitch(67)
  final val gs4: Pitch = Pitch(68)
  final val a4: Pitch  = Pitch(69)
  final val as4: Pitch = Pitch(70)
  final val b4: Pitch  = Pitch(71)
  final val c5: Pitch  = Pitch(72)
  final val cs5: Pitch = Pitch(73)
  final val d5: Pitch  = Pitch(74)
  final val ds5: Pitch = Pitch(75)
  final val e5: Pitch  = Pitch(76)
  final val f5: Pitch  = Pitch(77)
  final val fs5: Pitch = Pitch(78)
  final val g5: Pitch  = Pitch(79)
  final val gs5: Pitch = Pitch(80)
  final val a5: Pitch  = Pitch(81)
  final val as5: Pitch = Pitch(82)
  final val b5: Pitch  = Pitch(83)
  final val c6: Pitch  = Pitch(84)
  final val cs6: Pitch = Pitch(85)
  final val d6: Pitch  = Pitch(86)
  final val ds6: Pitch = Pitch(87)
  final val e6: Pitch  = Pitch(88)
  final val f6: Pitch  = Pitch(89)
  final val fs6: Pitch = Pitch(90)
  final val g6: Pitch  = Pitch(91)
  final val gs6: Pitch = Pitch(92)
  final val a6: Pitch  = Pitch(93)
  final val as6: Pitch = Pitch(94)
  final val b6: Pitch  = Pitch(95)
  final val c7: Pitch  = Pitch(96)
  final val cs7: Pitch = Pitch(97)
  final val d7: Pitch  = Pitch(98)
  final val ds7: Pitch = Pitch(99)
  final val e7: Pitch  = Pitch(100)
  final val f7: Pitch  = Pitch(101)
  final val fs7: Pitch = Pitch(102)
  final val g7: Pitch  = Pitch(103)
  final val gs7: Pitch = Pitch(104)
  final val a7: Pitch  = Pitch(105)
  final val as7: Pitch = Pitch(106)
  final val b7: Pitch  = Pitch(107)
  final val c8: Pitch  = Pitch(108)

  final val c: Pitch  = c4
  final val cs: Pitch = cs4
  final val d: Pitch  = d4
  final val ds: Pitch = ds4
  final val e: Pitch  = e4
  final val f: Pitch  = f4
  final val fs: Pitch = fs4
  final val g: Pitch  = g4
  final val gs: Pitch = gs4
  final val a: Pitch  = a4
  final val as: Pitch = as4
  final val b: Pitch  = b4

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
