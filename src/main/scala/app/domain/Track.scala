package app.domain

import app.midi.*
import app.domain.Generator
import app.shared.IsValid

case class Track(
                  channel: IsValid[Channel],
                  timeGen: Generator[Tick],
                  durGen: Generator[Tick],
                  noteGen: Generator[Note],
                  velGen: Option[Generator[Velocity]] = None
)
