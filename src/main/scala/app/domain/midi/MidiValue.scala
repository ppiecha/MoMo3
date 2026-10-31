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
  final val a0   = 21
  final val as0  = 22
  final val b0   = 23
  final val c1   = 24
  final val cs1  = 25
  final val d1   = 26
  final val ds1  = 27
  final val e1   = 28
  final val f1   = 29
  final val fs1  = 30
  final val g1   = 31
  final val gs1  = 32
  final val a1   = 33
  final val as1  = 34
  final val b1   = 35
  final val c2   = 36
  final val cs2  = 37
  final val d2   = 38
  final val ds2  = 39
  final val e2   = 40
  final val f2   = 41
  final val fs2  = 42
  final val g2   = 43
  final val gs2  = 44
  final val a2   = 45
  final val as2  = 46
  final val b2   = 47
  final val c3   = 48
  final val cs3  = 49
  final val d3   = 50
  final val ds3  = 51
  final val e3   = 52
  final val f3   = 53
  final val fs3  = 54
  final val g3   = 55
  final val gs3  = 56
  final val a3   = 57
  final val as3  = 58
  final val b3   = 59
  final val c4   = 60
  final val cs4  = 61
  final val d4   = 62
  final val ds4  = 63
  final val e4   = 64
  final val f4   = 65
  final val fs4  = 66
  final val g4   = 67
  final val gs4  = 68
  final val a4   = 69
  final val as4  = 70
  final val b4   = 71
  final val c5   = 72
  final val cs5  = 73
  final val d5   = 74
  final val ds5  = 75
  final val e5   = 76
  final val f5   = 77
  final val fs5  = 78
  final val g5   = 79
  final val gs5  = 80
  final val a5   = 81
  final val as5  = 82
  final val b5   = 83
  final val c6   = 84
  final val cs6  = 85
  final val d6   = 86
  final val ds6  = 87
  final val e6   = 88
  final val f6   = 89
  final val fs6  = 90
  final val g6   = 91
  final val gs6  = 92
  final val a6   = 93
  final val as6  = 94
  final val b6   = 95
  final val c7   = 96
  final val cs7  = 97
  final val d7   = 98
  final val ds7  = 99
  final val e7   = 100
  final val f7   = 101
  final val fs7  = 102
  final val g7   = 103
  final val gs7  = 104
  final val a7   = 105
  final val as7  = 106
  final val b7   = 107
  final val c8   = 108

  final val c  = c4
  final val cs = cs4
  final val d  = d4
  final val ds = ds4
  final val e  = e4
  final val f  = f4
  final val fs = fs4
  final val g  = g4
  final val gs = gs4
  final val a  = a4
  final val as = as4
  final val b  = b4

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
