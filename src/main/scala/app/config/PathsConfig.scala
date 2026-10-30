package app.config

import pureconfig.ConfigReader

case class PathsConfig(
  tracks: String,
  musicFile: String,
  commonFile: String,
  pollingInterval: Int
) derives ConfigReader
