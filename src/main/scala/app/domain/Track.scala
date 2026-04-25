package app.domain

import app.midi.*
import app.domain.Generator

case class Track(
    channel: Channel,
    timeGen: Generator[Tick],
    durGen: Generator[Tick],
    noteGen: Generator[Note]
)
