package app.midi

import app.midi.Message.* 
import javax.sound.midi.ShortMessage

final case class GeneratedMessage(
  note: NoteMessage,
  program: Option[ProgramMessage],
  control: Option[ControlMessage]
) {
  def toMidiMessages(channel: Channel): List[ShortMessage] = 
    program.toList.flatMap(_.toMidiMessages(channel)) ++
    control.toList.flatMap(_.toMidiMessages(channel)) ++
    note.toMidiMessages(channel)
}
