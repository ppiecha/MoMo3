package app.config

import pureconfig.ConfigReader

case class SynthConfig(
  soundFontPath: String,
  fluidsynthPath: String
) derives ConfigReader
