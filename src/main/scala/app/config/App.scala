package app.config

import app.domain.*
import cats.data.{NonEmptyChain, Reader, ValidatedNec}
import scala.io.StdIn
import fs2.*
import cats.data.*
import cats.effect.*

type IsValid[A] = ValidatedNec[ValidationError, A]

// env => F[Either[DomainError, A]]
type App[F[_], A] = EitherT[[X] =>> ReaderT[F, Environment, X], DomainError, A]

def ask[F[_]: Async]: App[F, Environment] = EitherT.liftF(ReaderT.ask[F, Environment])

// Lift from F into App
def liftF[F[_]: Async, A](fa: F[A]): App[F, A] = EitherT.liftF(ReaderT.liftF(fa))

def liftFPure[F[_]: Async, A](a: A): App[F, A] = EitherT.liftF(ReaderT.liftF(Async[F].pure(a)))

def raise[F[_]: Async](e: DomainError): App[F, Nothing] = EitherT.leftT(e)


