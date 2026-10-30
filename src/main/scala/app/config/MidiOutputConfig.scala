package app.config

import pureconfig.ConfigReader

case class MidiOutputConfig(loopMidiPortName: String) derives ConfigReader
