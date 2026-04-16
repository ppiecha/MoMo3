package app.application

import javax.sound.midi.MidiEvent

object TrackPrinter {
  def makeString(events: LazyList[MidiEvent], number: Int = 10): String = {
    s"List(${events.take(number).map(_.toString).mkString("\n  ", ",\n  ", "\n")}${
        if (events.size > number) ", ..." else ""
      })"
  }
}
