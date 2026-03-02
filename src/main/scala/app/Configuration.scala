package app

import app.midi.*
import cats.data.{NonEmptyChain, Reader, ValidatedNec}
import scala.io.StdIn
import fs2.*
import javax.sound.midi.*
import cats.data.*
import cats.effect.*

type IsValid[A] = ValidatedNec[ValidationError, A]

// env => F[Either[DomainError, A]]
type App[F[_], A] = EitherT[[X] =>> ReaderT[F, Env, X], DomainError, A]

def ask[F[_]: Async]: App[F, Env] = EitherT.liftF(ReaderT.ask[F, Env])

// Lift from F into App
def liftF[F[_]: Async, A](fa: F[A]): App[F, A] = EitherT.liftF(ReaderT.liftF(fa))

def raise[F[_]: Async](e: DomainError): App[F, Nothing] = EitherT.leftT(e) 

type MidiStream[F[_]] = Stream[F, MidiMessage]

case class Env(bpm: Bpm, console: Console, soundFontPath: String = "sf/soundfont.sf2")

trait Console {
  val debug = true
  def println(msg: String): Unit
  def readLine(): String
}

val consoleImpl = new Console {
  override def println(msg: String): Unit = if debug then Predef.println(msg)
  override def readLine(): String         = StdIn.readLine()
}
