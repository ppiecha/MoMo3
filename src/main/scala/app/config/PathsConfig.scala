package app.config

import pureconfig.ConfigReader

case class PathsConfig(
  tracks: String = "tracks",
  musicFile: String = "music.scala",
  commonFile: String = "common.scala",
  pollingInterval: Int = 500
) derives ConfigReader
