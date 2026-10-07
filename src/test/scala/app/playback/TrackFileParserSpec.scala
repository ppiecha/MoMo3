package app.playback

import app.domain.DomainError
import munit.FunSuite

import java.lang.reflect.InvocationTargetException

class TrackFileParserSpec extends FunSuite {

  test("extractCompilationError parses file line and message from angle-bracket location") {
    val raw =
      "class dotty.tools.dotc.reporting.Diagnostic$Error at projects\\test1\\Drums.scala:<154..154> L6: an identifier expected, but ')' found"

    val parsed = TrackFileParser.extractCompilationError(raw)

    assertEquals(parsed.fileName, "Drums.scala")
    assertEquals(parsed.lineNumber, 6)
    assertEquals(parsed.message, "an identifier expected, but ')' found")
  }

  test("extractCompilationError parses file line and message from square-bracket location") {
    val raw =
      "class dotty.tools.dotc.reporting.Diagnostic$Error at C:\\temp\\Piano.scala:[21..32..41] L1: type TrackFile is not a member of app.syntax"

    val parsed = TrackFileParser.extractCompilationError(raw)

    assertEquals(parsed.fileName, "Piano.scala")
    assertEquals(parsed.lineNumber, 1)
    assertEquals(parsed.message, "type TrackFile is not a member of app.syntax")
  }

  test("extractCompilationError falls back to unknown format when diagnostic cannot be parsed") {
    val raw = "some unstructured compiler output"

    val parsed = TrackFileParser.extractCompilationError(raw)

    assertEquals(parsed.fileName, "unknown")
    assertEquals(parsed.lineNumber, -1)
    assertEquals(parsed.message, "some unstructured compiler output")
  }

  test("catchAllNec unwraps invocation target exception and keeps cause message") {
    val result = TrackFileParser.catchAllNec[Int] {
      throw new InvocationTargetException(new IllegalStateException("kill requested"))
    }

    result match
      case cats.data.Validated.Invalid(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.MusicFileParseFailed("IllegalStateException: kill requested"))
        )
      case _ =>
        fail("Expected invalid result")
  }

  test("catchAllNec keeps non-empty fallback when exception has null message") {
    val result = TrackFileParser.catchAllNec[Int] {
      throw new RuntimeException(null: String)
    }

    result match
      case cats.data.Validated.Invalid(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.MusicFileParseFailed("java.lang.RuntimeException"))
        )
      case _ =>
        fail("Expected invalid result")
  }
}
