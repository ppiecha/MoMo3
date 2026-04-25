package app.application

import app.shared.ErrorOr
import app.domain.*

final case class CompiledTrack(events: LazyList[ErrorOr[AbsoluteMidiEvent]]) 
