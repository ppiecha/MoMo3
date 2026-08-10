package app.application

import app.domain.*

final case class CompiledTrack(events: LazyList[Either[DomainError, AbsoluteMidiEvent]])
