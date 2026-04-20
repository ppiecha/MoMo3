package app.config

import app.domain.DomainError

case class DomainException(error: DomainError) extends RuntimeException(error.toString)
