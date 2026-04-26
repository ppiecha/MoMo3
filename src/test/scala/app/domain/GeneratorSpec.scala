package app.domain

import app.midi.*
import app.config.*
import app.domain.*
import scala.concurrent.duration.*
import munit.FunSuite
import cats.syntax.all.*

class GeneratorSpec extends FunSuite {

  test("Tick.fromDouble should convert 8 double note duration to ticks based on PPQ") {
    val testEnv   = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
    val validated = Tick.fromDouble(8, testEnv.ppq)
    assert(validated.isValid)
    assertEquals(validated, Tick.fromInt(480))
  }

  test("toMillis should convert 4d/3 double note duration to finite duration in millis based on BPM") {
    val testEnv   = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
    val validated = Tick.fromDouble(4d/3, testEnv.ppq).map(_.toMillis(testEnv))
    assert(validated.isValid)
    assertEquals(validated, 3.seconds.validNec[ValidationError])
  }

}
