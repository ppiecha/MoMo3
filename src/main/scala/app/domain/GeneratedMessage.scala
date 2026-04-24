package app.domain

import Message.*

final case class GeneratedMessage(
  note: Option[NoteMessage],
  program: Option[ProgramMessage],
  control: Option[ControlMessage]
) {
  def toCommands(channel: Channel): List[MidiCommand] = 
    program.toList.flatMap(_.toCommands(channel)) ++
    control.toList.flatMap(_.toCommands(channel)) ++
    note.toList.flatMap(_.toCommands(channel))
}
