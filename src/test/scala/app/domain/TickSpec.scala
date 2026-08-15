package app.domain

import app.config.*
import app.domain.*
import cats.data.Validated

import scala.concurrent.duration.*
import munit.FunSuite
import cats.syntax.all.*

class TickSpec extends FunSuite {

  test("Tick.fromDouble should convert 8 double note duration to ticks based on PPQ") {
    val testEnv = Environment.from(bpm = 60)
    testEnv match {
      case Validated.Valid(env) =>
        val validatedTick = Tick.fromDouble(8, env.timingContext.ppq)
        assert(validatedTick.isValid)
        assertEquals(validatedTick, Tick.fromInt(480))
      case Validated.Invalid(e) => fail(s"Environment creation failed with errors: ${e.toList.mkString(", ")}")
    }
  }

  test("toMillis should convert 4d/3 double note duration to finite duration in millis based on BPM") {
    val testEnv = Environment.from(bpm = 60)
    testEnv match {
      case Validated.Valid(env) =>
        val validated =
          Tick.fromDouble(4d / 3, env.timingContext.ppq).map(_.toMillis(env.timingContext.ppq, env.timingContext.bpm))
        assert(validated.isValid)
        assertEquals(validated, 3.seconds.validNec[ValidationError])
      case Validated.Invalid(e) => fail(s"Environment creation failed with errors: ${e.toList.mkString(", ")}")
    }
  }

}
