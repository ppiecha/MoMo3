package app.domain

import app.midi.*
import app.domain.Generator
import cats.data.ValidatedNec
import app.domain.ValidationError

case class Track(
  channel: ValidatedNec[ValidationError, Channel],
  timeGen: Generator[Tick],
  durGen: Generator[Tick],
  noteGen: Generator[Note],
  velGen: Option[Generator[Velocity]] = None
)
