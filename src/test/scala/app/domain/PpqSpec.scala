package app.domain

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import cats.data.ValidatedNec
import cats.syntax.all.*

class PpqSpec extends FunSuite with ScalaCheckSuite {

  test("Ppq.DEFAULT_VALUE should be 960") {
    assertEquals(Ppq.DEFAULT_VALUE, 960)
  }

  test("Ppq.DEFAULT_PPQ should have value 960") {
    assertEquals(Ppq.DEFAULT_PPQ.value, 960)
  }

  test("Ppq.from should accept positive values") {
    val result: ValidatedNec[ValidationError, Ppq] = Ppq.from(480)
    assert(result.isValid)
    assertEquals(result.map(_.value).getOrElse(-1), 480)
  }

  test("Ppq.from should reject zero") {
    val result: ValidatedNec[ValidationError, Ppq] = Ppq.from(0)
    assert(result.isInvalid)
  }

  test("Ppq.from should reject negative values") {
    val result: ValidatedNec[ValidationError, Ppq] = Ppq.from(-480)
    assert(result.isInvalid)
  }

  property("Ppq.from accepts all positive values") {
    forAll(Gen.choose(1, 10000)) { value =>
      val result: ValidatedNec[ValidationError, Ppq] = Ppq.from(value)
      assert(result.isValid)
      assertEquals(result.map(_.value).getOrElse(-1), value)
    }
  }

  property("Ppq.from rejects all non-positive values") {
    forAll(Gen.choose(Int.MinValue, 0)) { value =>
      val result: ValidatedNec[ValidationError, Ppq] = Ppq.from(value)
      assert(result.isInvalid)
    }
  }

  test("Ppq.from should accept common PPQ values") {
    val commonPpqs = List(96, 192, 240, 480, 960, 1920)
    commonPpqs.foreach { ppq =>
      val result = Ppq.from(ppq)
      assert(result.isValid)
    }
  }

}
