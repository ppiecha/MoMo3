package app.domain

import app.config._
import app.domain._
import cats.data.Validated
import cats.syntax.all._
import munit.FunSuite
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

import scala.concurrent.duration._

class TickSpec extends FunSuite with ScalaCheckSuite {

  private val timingContext = valid(TimingContext.from(960, 60))
  given TimingContext       = timingContext

  test("Tick.fromDouble should convert 8 double note duration to ticks based on PPQ") {
    val testEnv = Environment.from(bpm = 60)
    testEnv match {
      case Right(env) =>
        val validatedTick = Tick.from(8)(using env.timingContext)
        assert(validatedTick.isValid)
        assertEquals(validatedTick.map(_.value), 480.validNec[DomainError])
      case Left(err) => fail(s"Environment creation failed with errors: $err")
    }
  }

  test("toMillis should convert 4d/3 double note duration to finite duration in millis based on BPM") {
    val testEnv = Environment.from(bpm = 60)
    testEnv match {
      case Right(env) =>
        val validated =
          Tick.from(4d / 3)(using env.timingContext).map(_.toMillis(using env.timingContext))
        assert(validated.isValid)
        assertEquals(validated, 3.seconds.validNec[DomainError])
      case Left(err) => fail(s"Environment creation failed with errors: $err")
    }
  }

  test("Tick.zero should be 0") {
    assertEquals(Tick.Zero.value, 0)
  }

  test("Tick.from should accept non-negative note durations") {
    val result = Tick.from(100)
    assert(result.isValid)
    assertEquals(result.map(_.value).getOrElse(-1), 38)
  }

  test("Tick.from should reject negative note durations") {
    val result = Tick.from(-1)
    assert(result.isInvalid)
  }

  property("Tick.from accepts all non-negative note durations") {
    forAll(Gen.choose(0, 100000)) { value =>
      val result = Tick.from(value)
      assert(result.isValid)
      val expectedTicks = if value == 0 then 0 else (timingContext.ppq.value.toDouble * 4 / value).toInt
      assertEquals(result.map(_.value).getOrElse(-1), expectedTicks)
    }
  }

  property("Tick.from rejects all negative note durations") {
    forAll(Gen.negNum[Int]) { value =>
      val result = Tick.from(value)
      assert(result.isInvalid)
    }
  }

  test("Tick addition should work correctly") {
    val tick1  = Tick.from(4).getOrElse(Tick.Zero)
    val tick2  = Tick.from(8).getOrElse(Tick.Zero)
    val result = tick1 + tick2
    assertEquals(result.value, 1440)
  }

  property("Tick addition is associative") {
    forAll(Gen.choose(0, 10000), Gen.choose(0, 10000), Gen.choose(0, 10000)) { (a, b, c) =>
      val tick1   = Tick.from(a).getOrElse(Tick.Zero)
      val tick2   = Tick.from(b).getOrElse(Tick.Zero)
      val tick3   = Tick.from(c).getOrElse(Tick.Zero)
      val result1 = (tick1 + tick2) + tick3
      val result2 = tick1 + (tick2 + tick3)
      assertEquals(result1.value, result2.value)
    }
  }

  test("Tick subtraction should work correctly") {
    val tick1  = Tick.from(4).getOrElse(Tick.Zero)
    val tick2  = Tick.from(8).getOrElse(Tick.Zero)
    val result = tick1 - tick2
    assertEquals(result.value, 480)
  }

  private def valid[A](validated: cats.data.ValidatedNec[DomainError, A]): A =
    validated match {
      case Validated.Valid(value)    => value
      case Validated.Invalid(errors) => fail(s"Expected a valid value but got errors: $errors")
    }
}
