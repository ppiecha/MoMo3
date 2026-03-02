package app

import javax.sound.midi.Soundbank
sealed trait DomainError extends Product with Serializable

enum ValidationError extends DomainError {
  case InvalidPpq(value: Int) extends ValidationError
  case InvalidBpm(value: Int) extends ValidationError
  case InvalidMidiValue(value: Int) extends ValidationError
  case InvalidChannel(value: Int) extends ValidationError
  case InvalidTick(value: Long) extends ValidationError
  case InvalidEvent(error: String) extends ValidationError
  case InvalidEvents(errors: List[InvalidEvent]) extends ValidationError
}

enum FileError extends DomainError {
  case FileNotFound(path: String) extends FileError
}

enum MidiError extends DomainError {
  case SoundbankObjectNotDefined(sb: Soundbank) extends MidiError
  case SoundbankNotSupported(sb: Soundbank) extends MidiError
  case SoundbankLoadFailed(sb: Soundbank) extends MidiError
}