package app.config

import pureconfig.ConfigReader

case class MidiConfig(
  velocity: Int = 100,
  volume: Int = 100,
  chorus: Int = 0,
  reverb: Int = 0
) derives ConfigReader
