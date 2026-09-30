package app.domain

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all._

def validationToDomainError(errors: NonEmptyChain[ValidationError]): NonEmptyChain[DomainError] =
  errors.map(ve => DomainError.ValidationFailed(ve))

enum ValidationError {
  case InvalidPpq(value: Int)
  case InvalidBpm(value: Int)
  case InvalidTick(value: Int)
  case InvalidMidiValue(value: Int)
  case InvalidChannel(value: Int)
  case InvalidVelocity(value: Int)
  case InvalidTimeValue(value: Long)
  case InvalidMessage(error: String)
  case InvalidEvent(errors: NonEmptyChain[ValidationError])
  case InvalidConfig(errors: NonEmptyList[ValidationError])
  case InvalidPort(portName: String)
  case EmptyListInSlidingWindow
  case ChannelMismatch(channel1: Channel, channel2: Channel)
  case NegativeDuration(duration: Double)
  case DurationMismatch(actual: Double, expected: Double)
  case EmptyTracks

  override def toString: String = this match {
    case InvalidPpq(value)                   => s"Invalid PPQ value: $value"
    case InvalidBpm(value)                   => s"Invalid BPM value: $value"
    case InvalidTick(value)                  => s"Invalid tick value: $value"
    case InvalidMidiValue(value)             => s"Invalid MIDI value: $value"
    case InvalidChannel(value)               => s"Invalid channel value: $value"
    case InvalidVelocity(value)              => s"Invalid velocity value: $value"
    case InvalidTimeValue(value)             => s"Invalid time value: $value"
    case InvalidMessage(error)               => s"Invalid message: $error"
    case InvalidEvent(errors)                => s"Invalid event with errors: ${errors.toList.mkString(", ")}"
    case InvalidConfig(errors)               => s"Invalid config with errors: ${errors.toList.mkString(", ")}"
    case InvalidPort(portName)               => s"Invalid port name: $portName"
    case EmptyListInSlidingWindow            => "Empty list in sliding window"
    case ChannelMismatch(channel1, channel2) => s"Channel mismatch between $channel1 and $channel2"
    case NegativeDuration(duration)          => s"Negative duration: $duration"
    case DurationMismatch(actual, expected)  => s"Duration mismatch: actual=$actual, expected=$expected"
    case EmptyTracks                         => "No tracks provided for playback plan."
  }

}

enum DomainError {
  case ValidationFailed(err: ValidationError)
  case PlaybackFailed(msg: String)
  case TrackFileParseFailed(error: String)
  case MusicFileParseFailed(error: String)

  override def toString: String = this match {
    case ValidationFailed(err)       => s"Validation failed: $err"
    case PlaybackFailed(msg)         => s"Playback failed: $msg"
    case TrackFileParseFailed(error) => s"Track file parse failed: $error"
    case MusicFileParseFailed(error) => s"Music file parse failed: $error"

  }

}
