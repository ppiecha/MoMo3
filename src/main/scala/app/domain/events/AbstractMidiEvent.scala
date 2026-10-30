package app.domain

sealed trait AbstractMidiEvent
object AbstractMidiEvent {
  final case class AbsoluteMidiEvent(at: Tick, command: MidiCommand) extends AbstractMidiEvent
  case object RestEvent extends AbstractMidiEvent
}
