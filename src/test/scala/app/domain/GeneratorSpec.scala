package app.domain

import app.midi.*
import app.config.*
import app.domain.Generator.doubleToFiniteDuration
import scala.concurrent.duration.*
import munit.FunSuite

class GeneratorSpec extends FunSuite {

  test("doubleToFiniteDuration should convert 8 double note duration to finite duration in millis based on BPM") {
    val testEnv   = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
    val validated = doubleToFiniteDuration(8, testEnv)
    assert(validated.isValid)
    assertEquals(validated.getOrElse(0.millis), 500.millis)
  }

  test("doubleToFiniteDuration should convert 4d/3 double note duration to finite duration in millis based on BPM") {
    val testEnv   = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
    val validated = doubleToFiniteDuration(4d/3, testEnv)
    assert(validated.isValid)
    assertEquals(validated.getOrElse(0.millis), 3.seconds)
  }

}
