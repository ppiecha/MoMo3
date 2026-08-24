package app.tracks

import munit.FunSuite

class TrackFileParserSpec extends FunSuite {

  test("parse valid track with velocity") {
    val content =
      """|# Test track
         |channel=0
         |time=4,4,2
         |duration=1,1,2
         |notes=60,64,67
         |velocity=64,64,64
         |""".stripMargin

    assert(TrackFileParser.parse(content).isRight)
  }

  test("parse valid track without velocity") {
    val content =
      """|channel=1
         |time=8,8
         |duration=8,8
         |notes=36,39
         |""".stripMargin

    val result = TrackFileParser.parse(content)
    assert(result.isRight)
    result.foreach(t => assertEquals(t.velGen, None))
  }

  test("ignore comments and blank lines") {
    val content =
      """|# comment
         |
         |channel=0
         |# another comment
         |time=4,4
         |duration=4,4
         |notes=60,64
         |""".stripMargin

    assert(TrackFileParser.parse(content).isRight)
  }

  test("fail on missing channel") {
    val content =
      """|time=4,4
         |duration=4,4
         |notes=60,64
         |""".stripMargin

    assertEquals(TrackFileParser.parse(content), Left(TrackFileParser.ParseError.MissingKey("channel")))
  }

  test("fail on missing time") {
    val content =
      """|channel=0
         |duration=4,4
         |notes=60,64
         |""".stripMargin

    assertEquals(TrackFileParser.parse(content), Left(TrackFileParser.ParseError.MissingKey("time")))
  }

  test("fail on missing duration") {
    val content =
      """|channel=0
         |time=4,4
         |notes=60,64
         |""".stripMargin

    assertEquals(TrackFileParser.parse(content), Left(TrackFileParser.ParseError.MissingKey("duration")))
  }

  test("fail on missing notes") {
    val content =
      """|channel=0
         |time=4,4
         |duration=4,4
         |""".stripMargin

    assertEquals(TrackFileParser.parse(content), Left(TrackFileParser.ParseError.MissingKey("notes")))
  }

  test("fail on invalid channel value") {
    val content =
      """|channel=abc
         |time=4,4
         |duration=4,4
         |notes=60,64
         |""".stripMargin

    assertEquals(TrackFileParser.parse(content), Left(TrackFileParser.ParseError.InvalidValue("channel", "abc")))
  }

  test("fail on invalid time value") {
    val content =
      """|channel=0
         |time=4,x
         |duration=4,4
         |notes=60,64
         |""".stripMargin

    assertEquals(TrackFileParser.parse(content), Left(TrackFileParser.ParseError.InvalidValue("time", "x")))
  }

  test("parse floating point time values") {
    val content =
      """|channel=0
         |time=4,1.333,2
         |duration=4,1.333,2
         |notes=60,64,67
         |""".stripMargin

    assert(TrackFileParser.parse(content).isRight)
  }

  test("fail on invalid velocity value") {
    val content =
      """|channel=0
         |time=4,4
         |duration=4,4
         |notes=60,64
         |velocity=abc
         |""".stripMargin

    assertEquals(TrackFileParser.parse(content), Left(TrackFileParser.ParseError.InvalidValue("velocity", "abc")))
  }

  test("parse percussion channel 9") {
    val content =
      """|channel=9
         |time=8,8,8,8
         |duration=8,8,8,8
         |notes=36,36,39,36
         |""".stripMargin

    assert(TrackFileParser.parse(content).isRight)
  }
}
