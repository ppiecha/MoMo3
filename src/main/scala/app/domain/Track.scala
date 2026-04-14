package app.domain

import app.midi.*
import app.domain.Generator

case class Track(
    channel: Channel,
    timeGen: Generator[Time],
    durGen: Generator[Time],
    noteGen: Generator[Note]
)
