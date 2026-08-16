package app.domain

import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import cats.data.ValidatedNec
import cats.syntax.all.*

class BpmSpec extends FunSuite with ScalaCheckSuite {

  test("Bpm.from should accept positive values") {
    val result: ValidatedNec[ValidationError, Bpm] = Bpm.from(120)
    assert(result.isValid)
    assertEquals(result.map(_.value).getOrElse(-1), 120)
  }

  test("Bpm.from should reject zero") {
    val result: ValidatedNec[ValidationError, Bpm] = Bpm.from(0)
    assert(result.isInvalid)
  }

  test("Bpm.from should reject negative values") {
    val result: ValidatedNec[ValidationError, Bpm] = Bpm.from(-60)
    assert(result.isInvalid)
  }

  property("Bpm.from accepts all positive values") {
    forAll(Gen.choose(1, 300)) { value =>
      val result: ValidatedNec[ValidationError, Bpm] = Bpm.from(value)
      assert(result.isValid)
      assertEquals(result.map(_.value).getOrElse(-1), value)
    }
  }

  property("Bpm.from rejects all non-positive values") {
    forAll(Gen.choose(Int.MinValue, 0)) { value =>
      val result: ValidatedNec[ValidationError, Bpm] = Bpm.from(value)
      assert(result.isInvalid)
    }
  }

  test("Bpm.from should accept common BPM values") {
    val commonBpms = List(60, 90, 120, 140, 180, 240)
    commonBpms.foreach { bpm =>
      val result = Bpm.from(bpm)
      assert(result.isValid)
    }
  }

}
