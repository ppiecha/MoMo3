package app.logging

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.pattern.color.ForegroundCompositeConverterBase

// debug -> grey, info -> green, warning -> yellow, error -> red
class LevelColorConverter extends ForegroundCompositeConverterBase[ILoggingEvent] {
  override def getForegroundColorCode(event: ILoggingEvent): String =
    event.getLevel.toInt match {
      case Level.ERROR_INT => "31" // red
      case Level.WARN_INT  => "33" // yellow
      case Level.INFO_INT  => "32" // green
      case Level.DEBUG_INT => "90" // grey
      case _               => "39" // default
    }
}
