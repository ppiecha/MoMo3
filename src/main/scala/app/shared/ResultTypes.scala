package app.shared

import app.domain.*
import cats.data.ValidatedNec


type IsValid[A] = ValidatedNec[ValidationError, A]

type ErrorOr[A] = Either[DomainError, A]
