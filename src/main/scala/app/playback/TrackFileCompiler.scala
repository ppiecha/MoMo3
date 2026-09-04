package app.playback

import app.domain.Track
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import dotty.tools.dotc.*
import dotty.tools.dotc.reporting.*
import dotty.tools.dotc.core.Contexts.*
import cats.syntax.all.*

import java.nio.file.{Files, Paths}
import scala.reflect.Typeable
import scala.compiletime.summonFrom
import scala.compiletime.error

object TrackFileCompiler {

  def createTempDir(): String = {
    val dir = Files.createTempDirectory(null)
    dir.toFile.deleteOnExit()
    dir.toString
  }

  def compileFile(
    scalaFile: String,
    outDir: String,
    classpath: String = sys.props("java.class.path")
  ): ValidatedNec[String, String] = {

    val reporter = new StoreReporter()

    val args = Array(
      scalaFile,
      "-d",
      outDir,
      "-classpath",
      classpath
    )

    val driver = new Driver:
      override protected def newCompiler(using Context) =
        new Compiler

    driver.process(args, reporter)

    if reporter.hasErrors then
      NonEmptyChain
        .fromSeq(reporter.allErrors.map(_.toString))
        .getOrElse(NonEmptyChain.one("unknown error"))
        .invalid[String]
    else outDir.validNec[String]
  }

  private def evaluate[A](className: String, methodName: String, outDir: String)(using Typeable[A]): A = {
    val loader = new java.net.URLClassLoader(
      Array(new java.io.File(outDir).toURI.toURL),
      getClass.getClassLoader
    )
    val cls    = loader.loadClass(className + "$")
    val module = cls.getField("MODULE$").get(null)
    val method = cls.getMethod(methodName)
    val result = method.invoke(module)
    result.asInstanceOf[A]
  }

  inline def requireTypeable[T]: Typeable[T] =
    summonFrom {
      case t: Typeable[T] => t
      case _ =>
        error(
          "Cannot check type parameter T at runtime.\n" +
            "Provide an explicit type argument or a given Typeable[T]."
        )
    }

  inline def compileAndEvaluateFile[A](
    scalaFile: String,
    className: String,
    methodName: String,
    outDir: Either[() => String, String] = Left(createTempDir),
    classpath: String = sys.props("java.class.path")
  )(using Typeable[A]): ValidatedNec[String, A] = {
    requireTypeable[A]
    val tempDir = outDir.fold(_(), identity)
    compileFile(scalaFile, tempDir, classpath) match {
      case Validated.Valid(compiledDir) =>
        Validated
          .catchNonFatal(evaluate[A](className, methodName, compiledDir))
          .leftMap { e =>
            NonEmptyChain.one(
              s"Evaluation failed for file '$scalaFile', class '$className', method '$methodName': ${e.getClass.getSimpleName}: ${e.getMessage}"
            )
          }
      case Validated.Invalid(e) =>
        e.invalid
    }
  }

  inline def compileAndEvaluateFile(scalaFile: java.nio.file.Path): ValidatedNec[String, Track] =
    compileAndEvaluateFile[Track](
      scalaFile = scalaFile.toString,
      className = "Music",
      methodName = "play"
    )

}
