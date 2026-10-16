package app.syntax

import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.ValidatedNec
import cats.effect.IO

def toIorNec[E, A](v: ValidatedNec[E, A]): Ior[NonEmptyChain[E], A] =
  v.fold(Ior.left, Ior.right)

extension [E, A](ior: Ior[NonEmptyChain[E], Ior[NonEmptyChain[E], A]]) {
  def flatten: Ior[NonEmptyChain[E], A] =
    ior match {
      case Ior.Left(errors)                            => Ior.Left(errors)
      case Ior.Right(Ior.Left(errors))                 => Ior.Left(errors)
      case Ior.Right(Ior.Right(value))                 => Ior.Right(value)
      case Ior.Right(Ior.Both(errors, value))          => Ior.Both(errors, value)
      case Ior.Both(errors1, Ior.Left(errors2))        => Ior.Left(errors1 ++ errors2)
      case Ior.Both(errors1, Ior.Right(value))         => Ior.Both(errors1, value)
      case Ior.Both(errors1, Ior.Both(errors2, value)) => Ior.Both(errors1 ++ errors2, value)
    }
}

extension [E, A](ior: Ior[E, IO[A]]) {
  def sequenceIO: IO[Ior[E, A]] =
    ior match {
      case Ior.Right(ioA) =>
        ioA.map(Ior.right)
      case Ior.Left(e) =>
        IO.pure(Ior.left(e))
      case Ior.Both(e, ioA) =>
        ioA.map(a => Ior.both(e, a))
    }
}
