package app.config

import munit.FunSuite
import pureconfig.ConfigSource

class EnvironmentSpec extends FunSuite {

  test("Environment.load should load configuration from default application.conf") {
    val result = Environment.load()
    assert(result.isRight, s"Expected successful load, got $result")
    val env = result.getOrElse(fail("Could not get environment"))

    assertEquals(env.timingContext.bpm.value, 60)
    assertEquals(env.timingContext.ppq.value, 960)
    assertEquals(env.synthConfig.soundFontPath, "C:\\tools\\fluidsynth\\soundfonts\\soundfont.sf2")
    assertEquals(env.synthConfig.fluidsynthPath, "C:\\tools\\fluidsynth\\bin\\fluidsynth.exe")
    assertEquals(env.midiOutputConfig.loopMidiPortName, "ScalaToFluid")
    assertEquals(env.midiConfig.velocity, 100)
    assertEquals(env.midiConfig.volume, 100)
    assertEquals(env.midiConfig.chorus, 0)
    assertEquals(env.midiConfig.reverb, 0)
    assertEquals(env.pathsConfig.tracks, "demo-tracks")
    assertEquals(env.pathsConfig.musicFile, "music.scala")
    assertEquals(env.pathsConfig.commonFile, "common.scala")
  }

  test("Environment.load should load from custom HOCON string") {
    val hocon =
      """
        |timing-context {
        |  bpm = 120
        |  ppq = 480
        |}
        |synth-config {
        |  sound-font-path = "/path/to/soundfont.sf2"
        |  fluidsynth-path = "/usr/bin/fluidsynth"
        |}
        |midi-output-config {
        |  loop-midi-port-name = "CustomPort"
        |}
        |midi {
        |  velocity = 110
        |  volume = 90
        |  chorus = 10
        |  reverb = 20
        |}
        |paths {
        |  tracks = "custom-tracks"
        |  music-file = "custom-music.scala"
        |  common-file = "custom-common.scala"
        |}
        |""".stripMargin

    val result = Environment.load(ConfigSource.string(hocon))
    assert(result.isRight)
    val env = result.getOrElse(fail("Could not get environment"))

    assertEquals(env.timingContext.bpm.value, 120)
    assertEquals(env.timingContext.ppq.value, 480)
    assertEquals(env.synthConfig.soundFontPath, "/path/to/soundfont.sf2")
    assertEquals(env.synthConfig.fluidsynthPath, "/usr/bin/fluidsynth")
    assertEquals(env.midiOutputConfig.loopMidiPortName, "CustomPort")
    assertEquals(env.midiConfig.velocity, 110)
    assertEquals(env.midiConfig.volume, 90)
    assertEquals(env.midiConfig.chorus, 10)
    assertEquals(env.midiConfig.reverb, 20)
    assertEquals(env.pathsConfig.tracks, "custom-tracks")
    assertEquals(env.pathsConfig.musicFile, "custom-music.scala")
    assertEquals(env.pathsConfig.commonFile, "custom-common.scala")
  }

  test("Environment.load should return error for invalid config") {
    val hocon =
      """
        |timing-context {
        |  bpm = -10
        |  ppq = 960
        |}
        |""".stripMargin

    val result = Environment.load(ConfigSource.string(hocon))
    assert(result.isLeft)
  }

}
