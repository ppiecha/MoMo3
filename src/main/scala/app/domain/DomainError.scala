package app.domain

import cats.data.NonEmptyList

sealed trait DomainError extends Product with Serializable

enum ValidationError extends DomainError {
  case InvalidPpq(value: Int)                           
  case InvalidBpm(value: Int)                           
  case InvalidTick(value: Int)                          
  case InvalidMidiValue(value: Int)                     
  case InvalidChannel(value: Int)                       
  case InvalidTimeValue(value: Long)                    
  case InvalidMessage(error: String)                    
  case InvalidEvent(error: String)                      
  case EmptyListInSlidingWindow                         
}

enum FileError extends DomainError {
  case FileNotFound(path: String) extends FileError
}

enum MidiError extends DomainError {
  case PortNotFound(portName: String)
}
