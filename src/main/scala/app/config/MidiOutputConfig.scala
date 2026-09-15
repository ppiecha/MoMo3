package app.config

import pureconfig.ConfigReader

case class MidiOutputConfig(loopMidiPortName: String = "ScalaToFluid") derives ConfigReader
