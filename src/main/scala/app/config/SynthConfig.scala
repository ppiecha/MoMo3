package app.config

import pureconfig.ConfigReader

case class SynthConfig(
  soundFontPath: String = "C:\\tools\\fluidsynth\\soundfonts\\soundfont.sf2",
  fluidsynthPath: String = "C:\\tools\\fluidsynth\\bin\\fluidsynth.exe"
) derives ConfigReader
