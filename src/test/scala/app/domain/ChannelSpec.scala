package app.domain

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import cats.data.ValidatedNec
import cats.syntax.all.*

class ChannelSpec extends FunSuite with ScalaCheckSuite {

  test("Channel.from should accept 0") {
    val result: ValidatedNec[ValidationError, Channel] = Channel.from(0)
    assert(result.isValid)
    assertEquals(result.map(_.value).getOrElse(-1), 0)
  }

  test("Channel.from should accept 15") {
    val result: ValidatedNec[ValidationError, Channel] = Channel.from(15)
    assert(result.isValid)
    assertEquals(result.map(_.value).getOrElse(-1), 15)
  }

  test("Channel.from should accept all MIDI channels (0-15)") {
    (0 to 15).foreach { ch =>
      val result = Channel.from(ch)
      assert(result.isValid)
    }
  }

  test("Channel.from should reject negative values") {
    val result: ValidatedNec[ValidationError, Channel] = Channel.from(-1)
    assert(result.isInvalid)
  }

  test("Channel.from should reject values above 15") {
    val result: ValidatedNec[ValidationError, Channel] = Channel.from(16)
    assert(result.isInvalid)
  }

  property("Channel.from accepts all valid channels (0-15)") {
    forAll(Gen.choose(0, 15)) { value =>
      val result: ValidatedNec[ValidationError, Channel] = Channel.from(value)
      assert(result.isValid)
      assertEquals(result.map(_.value).getOrElse(-1), value)
    }
  }

  property("Channel.from rejects out-of-range values") {
    forAll(Gen.oneOf(Gen.negNum[Int], Gen.choose(16, Int.MaxValue))) { value =>
      val result: ValidatedNec[ValidationError, Channel] = Channel.from(value)
      assert(result.isInvalid)
    }
  }

  test("Channel.from should reject large positive values") {
    val result: ValidatedNec[ValidationError, Channel] = Channel.from(128)
    assert(result.isInvalid)
  }

}
