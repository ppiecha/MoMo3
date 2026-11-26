package app.config

import munit.FunSuite
import pureconfig.ConfigSource

class EnvironmentSpec extends FunSuite {

  private val testInput = new ConsoleInput {
    override def readLine(): String = ""
    override def readKey(): Char    = '\n'
  }

  test("Environment.load should load configuration from default application.conf") {
    val expectedConfig = ConfigSource.default.loadOrThrow[EnvironmentConfig]
    val result         = Environment.load(ConfigSource.default, testInput)
    assert(result.isRight, s"Expected successful load, got $result")
    val env = result.getOrElse(fail("Could not get environment"))

    assertEquals(
      env.timingContext.bpm.value,
      expectedConfig.timingContext.bpm
    )
    assertEquals(env.timingContext.ppq.value, 960)
    assertEquals(env.midiOutputConfig.loopMidiPortName, "ScalaToFluid")
    assertEquals(env.pathsConfig.tracks, expectedConfig.paths.tracks)
    assertEquals(env.pathsConfig.musicFile, expectedConfig.paths.musicFile)
    assertEquals(env.startPlaybackOnStartup, true)
  }

  test("Environment.load should load from custom HOCON string") {
    val hocon =
      """
        |timing-context {
        |  bpm = 120
        |  ppq = 480
        |}
        |midi-output-config {
        |  loop-midi-port-name = "CustomPort"
        |}
        |paths {
        |  tracks = "custom-tracks"
        |  music-file = "custom-music.scala"
        |  common-file = "custom-common.scala"
        |  polling-interval = 500
        |}
        |start-playback-on-startup = false
        |""".stripMargin

    val result = Environment.load(ConfigSource.string(hocon), testInput)
    assert(result.isRight)
    val env = result.getOrElse(fail("Could not get environment"))

    assertEquals(env.timingContext.bpm.value, 120)
    assertEquals(env.timingContext.ppq.value, 480)
    assertEquals(env.midiOutputConfig.loopMidiPortName, "CustomPort")
    assertEquals(env.pathsConfig.tracks, "custom-tracks")
    assertEquals(env.pathsConfig.musicFile, "custom-music.scala")
    assertEquals(env.pathsConfig.commonFile, "custom-common.scala")
    assertEquals(env.startPlaybackOnStartup, false)
  }

  test("Environment.load should return error for invalid config") {
    val hocon =
      """
        |timing-context {
        |  bpm = -10
        |  ppq = 960
        |}
        |""".stripMargin

    val result = Environment.load(ConfigSource.string(hocon), testInput)
    assert(result.isLeft)
  }

}
