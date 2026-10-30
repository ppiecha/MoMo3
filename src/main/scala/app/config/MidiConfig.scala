package app.config

import pureconfig.ConfigReader

case class MidiConfig(
  velocity: Int,
  volume: Int,
  chorus: Int,
  reverb: Int
) derives ConfigReader
