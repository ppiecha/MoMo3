package app.playback

import app.domain.DomainError
import munit.FunSuite

import java.nio.file.Files
import java.util.Comparator

class TrackFileParserSpec extends FunSuite {

  test("extractCompilationError parses file line and message from angle-bracket location") {
    val raw =
      "class dotty.tools.dotc.reporting.Diagnostic$Error at projects\\test1\\Drums9.scala:<154..154> L6: an identifier expected, but ')' found"

    val parsed = TrackFileParser.extractCompilationError(raw)

    assertEquals(parsed.fileName, "Drums9.scala")
    assertEquals(parsed.lineNumber, 6)
    assertEquals(parsed.message, "an identifier expected, but ')' found")
  }

  test("extractCompilationError parses file line and message from square-bracket location") {
    val raw =
      "class dotty.tools.dotc.reporting.Diagnostic$Error at C:\\temp\\Bass0.scala:[21..32..41] L1: type TrackFile is not a member of app.syntax"

    val parsed = TrackFileParser.extractCompilationError(raw)

    assertEquals(parsed.fileName, "Bass0.scala")
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

  test("compile errors include source path, file name, and class name") {
    val directory = Files.createTempDirectory("track-file-parser-context")
    val source    = directory.resolve("BrokenTrack.scala")
    val output    = Files.createDirectory(directory.resolve("classes"))
    Files.writeString(source, "object BrokenTrack { val broken = }")

    try {
      val result = TrackFileParser.compileFiles(
        Seq(source.toString),
        output.toString,
        TrackFileParser.resolvedClasspath()
      )

      result match {
        case cats.data.Validated.Invalid(errors) =>
          val message = errors.toChain.toList.mkString("\n")
          assert(message.contains(s"source='${source.toString}'"))
          assert(!message.contains("file='"))
          assert(!message.contains("class='"))
          assert(message.contains("line="))
        case _ =>
          fail("Expected the invalid track source to fail compilation")
      }
    } finally {
      val paths = Files.walk(directory)
      try paths.sorted(Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
      finally paths.close()
    }
  }

  test("evaluation errors include source path, file name, and class name") {
    val directory = Files.createTempDirectory("track-file-parser-evaluation-context")
    val source    = directory.resolve("BrokenEvaluation.scala")
    Files.writeString(source, "object BrokenEvaluation {}")

    try {
      val result = TrackFileParser.compileAndEvaluateFile[String](
        scalaFile = source.toString,
        className = TrackFileParser.classNameFromFilePath(source),
        methodName = "missingMethod",
        sourceFiles = Nil,
        classpath = TrackFileParser.resolvedClasspath()
      )

      result match {
        case cats.data.Validated.Invalid(errors) =>
          val message = errors.toChain.toList.mkString("\n")
          assert(message.contains(s"source='${source.toString}'"))
          assert(!message.contains("file='"))
          assert(!message.contains("class='"))
        case _ =>
          fail("Expected the missing method to fail evaluation")
      }
    } finally {
      val paths = Files.walk(directory)
      try paths.sorted(Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
      finally paths.close()
    }
  }

  test("catchAllNec unwraps invocation target exception and keeps cause message") {
    val parseInt = classOf[java.lang.Integer].getMethod("parseInt", classOf[String])
    val result =
      TrackFileParser.catchAllNec[AnyRef](parseInt.invoke(new Object(), "not-an-integer"))

    result match
      case cats.data.Validated.Invalid(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.MusicFileParseFailed("NumberFormatException: For input string: \"not-an-integer\""))
        )
      case _ =>
        fail("Expected invalid result")
  }

  test("catchAllNec keeps non-empty fallback when exception has null message") {
    val emptyIterator = classOf[java.util.Collections].getMethod("emptyIterator").invoke(new Object())
    val next = classOf[java.util.Iterator[?]].getMethod("next")
    val result = TrackFileParser.catchAllNec[AnyRef](next.invoke(emptyIterator))

    result match
      case cats.data.Validated.Invalid(errors) =>
        assertEquals(
          errors.toChain.toList,
          List(DomainError.MusicFileParseFailed("java.util.NoSuchElementException"))
        )
      case _ =>
        fail("Expected invalid result")
  }
}
