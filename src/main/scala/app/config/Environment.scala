package app.config

import app.domain.*
import scala.io.StdIn

case class Environment(
    ppq: Ppq,
    bpm: Bpm,
    input: Input,
    soundFontPath: String = "C:\\tools\\fluidsynth\\soundfonts\\soundfont.sf2",
    loopMidiPortName: String = "ScalaToFluid"
)

trait Input {
  def readLine(): String
}

val stdInput = new Input {
  override def readLine(): String = StdIn.readLine()
}
