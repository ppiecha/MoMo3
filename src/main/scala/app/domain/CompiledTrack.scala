package app.domain

final case class CompiledTrack(events: Seq[Either[DomainError, AbsoluteMidiEvent]])
