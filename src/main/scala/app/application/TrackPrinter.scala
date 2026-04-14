package app.application

object TrackPrinter {
  def makeString(compiledTrack: CompiledTrack, number: Int = 10): String = {
    val events = compiledTrack.midiEvents
    s"List(${events.take(number).map(_.toString).mkString("\n  ", ",\n  ", "\n")}${
        if (events.size > number) ", ..." else ""
      })"
  }
}
