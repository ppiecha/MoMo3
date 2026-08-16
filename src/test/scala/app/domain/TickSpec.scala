package app.domain

import app.config.*
import app.domain.*
import cats.data.Validated
import org.scalacheck.Prop.*
import org.scalacheck.Gen

import scala.concurrent.duration.*
import munit.{FunSuite, ScalaCheckSuite}
import cats.syntax.all.*

class TickSpec extends FunSuite with ScalaCheckSuite {

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

  test("Tick.zero should be 0") {
    assertEquals(Tick.zero.value, 0)
  }

  test("Tick.fromInt should accept non-negative values") {
    val result = Tick.fromInt(100)
    assert(result.isValid)
    assertEquals(result.map(_.value).getOrElse(-1), 100)
  }

  test("Tick.fromInt should reject negative values") {
    val result = Tick.fromInt(-1)
    assert(result.isInvalid)
  }

  property("Tick.fromInt accepts all non-negative values") {
    forAll(Gen.choose(0, 100000)) { value =>
      val result = Tick.fromInt(value)
      assert(result.isValid)
      assertEquals(result.map(_.value).getOrElse(-1), value)
    }
  }

  property("Tick.fromInt rejects all negative values") {
    forAll(Gen.negNum[Int]) { value =>
      val result = Tick.fromInt(value)
      assert(result.isInvalid)
    }
  }

  test("Tick addition should work correctly") {
    val tick1  = Tick.fromInt(100).getOrElse(Tick.zero)
    val tick2  = Tick.fromInt(50).getOrElse(Tick.zero)
    val result = tick1 + tick2
    assertEquals(result.value, 150)
  }

  property("Tick addition is associative") {
    forAll(Gen.choose(0, 10000), Gen.choose(0, 10000), Gen.choose(0, 10000)) { (a, b, c) =>
      val tick1   = Tick.fromInt(a).getOrElse(Tick.zero)
      val tick2   = Tick.fromInt(b).getOrElse(Tick.zero)
      val tick3   = Tick.fromInt(c).getOrElse(Tick.zero)
      val result1 = (tick1 + tick2) + tick3
      val result2 = tick1 + (tick2 + tick3)
      assertEquals(result1.value, result2.value)
    }
  }

  test("Tick subtraction should work correctly") {
    val tick1  = Tick.fromInt(100).getOrElse(Tick.zero)
    val tick2  = Tick.fromInt(30).getOrElse(Tick.zero)
    val result = tick1 - tick2
    assertEquals(result.value, 70)
  }

}
