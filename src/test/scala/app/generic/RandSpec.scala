package app.generic

import cats.data.NonEmptyList
import munit.FunSuite

class RandSpec extends FunSuite {

  test("pRand concatenates the requested number of randomly selected values") {
    val values = NonEmptyList.of(Seq(1), Seq(2), Seq(3))

    val result = Rand.pRand(values, 20)

    assertEquals(result.length, 20)
    assert(result.forall(List(1, 2, 3).contains))
  }

  test("pRand works with strings") {
    val values = NonEmptyList.of("a", "b", "c")

    val result = Rand.pRand(values, 5)

    assertEquals(result.length, 5)
    assert(result.forall(character => values.toList.mkString.contains(character)))
  }

  test("pWRand returns the requested number of values chosen from the input") {
    val values  = NonEmptyList.of("a", "b", "c")
    val weights = NonEmptyList.of(1.0, 2.0, 3.0)

    val result = Rand.pWRand(values, weights, 20)

    assertEquals(result.length, 20)
    assert(result.toList.forall(values.toList.contains))
  }

  test("pWRand rejects weights with a different length") {
    intercept[IllegalArgumentException] {
      Rand.pWRand(NonEmptyList.of("a", "b"), NonEmptyList.one(1.0), 1)
    }
  }

  test("pWRand rejects negative weights") {
    intercept[IllegalArgumentException] {
      Rand.pWRand(NonEmptyList.of("a", "b"), NonEmptyList.of(1.0, -1.0), 1)
    }
  }

  test("pWhite returns the requested number of values inside the inclusive range") {
    val result = Rand.pWhite(2, 5, 100)

    assertEquals(result.length, 100)
    assert(result.forall(value => value >= 2 && value <= 5))
  }

  test("pWhite supports a range containing a single value") {
    assertEquals(Rand.pWhite(7, 7, 5), Seq.fill(5)(7))
  }

  test("pShuffle returns each input element once per repetition") {
    val input  = Seq("a", "b", "c")
    val result = Rand.pShuffle(input, 4)

    assertEquals(result.length, input.length * 4)
    assertEquals(result.grouped(input.length).map(_.toSet).toList, List.fill(4)(input.toSet))
  }

  test("pShuffle returns an empty sequence for no repetitions") {
    assertEquals(Rand.pShuffle(Seq(1, 2, 3), 0), Seq.empty)
  }
}
