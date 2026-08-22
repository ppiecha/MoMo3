package app.application

import app.domain.*

final case class CompiledTrack(events: Seq[Either[DomainError, AbsoluteMidiEvent]])
