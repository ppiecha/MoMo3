package app.playback

import munit.FunSuite

class RepeatPolicySpec extends FunSuite {

  test("fixed policy with non-positive count falls back to none") {
    assertEquals(RepeatPolicy.fixed(0), RepeatPolicy.none)
    assertEquals(RepeatPolicy.fixed(-3), RepeatPolicy.none)
  }

  test("fixed policy decrements and eventually reaches none") {
    val start = RepeatPolicy.fixed(2)

    assertEquals(start.remaining, 2)
    assertEquals(start.shouldRepeat, true)
    assertEquals(start.next, RepeatPolicy.fixed(1))
    assertEquals(start.next.next, RepeatPolicy.none)
  }

  test("none and forever have stable next semantics") {
    assertEquals(RepeatPolicy.none.shouldRepeat, false)
    assertEquals(RepeatPolicy.none.next, RepeatPolicy.none)

    assertEquals(RepeatPolicy.forever.shouldRepeat, true)
    assertEquals(RepeatPolicy.forever.next, RepeatPolicy.forever)
  }
}
