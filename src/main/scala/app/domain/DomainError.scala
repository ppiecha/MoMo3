package app.domain

import cats.data.{NonEmptyChain, NonEmptyList}

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
}

enum DomainError {
  case ValidationFailed(err: ValidationError)
  case TrackCompilationFailed(err: ValidationError)
  case PlaybackFailed(msg: String)

}
