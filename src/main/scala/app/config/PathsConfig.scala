package app.config

import pureconfig.ConfigReader

case class PathsConfig(
  tracks: String = "demo-tracks",
  musicFile: String = "music.scala",
  commonFile: String = "common.scala"
) derives ConfigReader
