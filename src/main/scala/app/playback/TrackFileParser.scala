package app.playback

import app.domain.DomainError
import app.domain.Track
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all._
import dotty.tools.dotc._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.reporting._

import java.io.File
import java.net.URL
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import scala.compiletime.error
import scala.compiletime.summonFrom
import scala.reflect.Typeable

object TrackFileParser {

  private val pathSeparator: String = File.pathSeparator

  private def urlToClasspathEntry(url: URL): Option[String] =
    if url.getProtocol == "file" then scala.util.Try(Path.of(url.toURI).toString).toOption
    else None

  private def classLocation(clazz: Class[?]): Option[String] =
    Option(clazz.getProtectionDomain)
      .flatMap(pd => Option(pd.getCodeSource))
      .flatMap(cs => Option(cs.getLocation))
      .flatMap(urlToClasspathEntry)

  private def classLoaderEntries(loader: ClassLoader): List[String] = {
    def loop(current: ClassLoader): List[String] =
      if current == null then Nil
      else {
        val here = current match
          case urlLoader: URLClassLoader => urlLoader.getURLs.toList.flatMap(urlToClasspathEntry)
          case _                         => Nil
        here ++ loop(current.getParent)
      }

    loop(loader)
  }

  private def resolvedClasspath(): String = {
    val fromProperty =
      sys.props
        .get("java.class.path")
        .toList
        .flatMap(_.split(pathSeparator).toList)
        .filter(_.nonEmpty)

    val fromClassLoaders =
      classLoaderEntries(Thread.currentThread().getContextClassLoader) ++
        classLoaderEntries(getClass.getClassLoader)

    val anchors = List(
      classLocation(classOf[Track]),
      classLocation(classOf[app.syntax.TrackFile]),
      classLocation(classOf[cats.data.Validated[?, ?]]),
      classLocation(classOf[scala.deriving.Mirror]),
      classLocation(classOf[scala.collection.immutable.List[?]]),
      classLocation(classOf[dotty.tools.dotc.Driver])
    ).flatten

    val primary =
      (fromClassLoaders ++ anchors).distinct

    val fallback =
      fromProperty.distinct

    val entries = if primary.nonEmpty then primary else fallback
    entries.mkString(pathSeparator)
  }

  def classNameFromFilePath(path: Path): String = {
    val fileName = path.getFileName.toString
    if !fileName.endsWith(".scala") then
      throw new IllegalArgumentException(s"File '$fileName' is not a Scala source file.")
    val dotIndex = fileName.lastIndexOf('.')
    if (dotIndex == -1) fileName
    else fileName.substring(0, dotIndex)
  }

  def compileFile(
    scalaFile: String,
    outDir: String,
    classpath: String = resolvedClasspath()
  ): ValidatedNec[DomainError, String] = {

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
        .fromSeq(reporter.allErrors.map(e => DomainError.TrackFileParseFailed(e.toString)))
        .getOrElse(NonEmptyChain.one(DomainError.TrackFileParseFailed("unknown error")))
        .invalid[String]
    else outDir.validNec[DomainError]
  }

  private def evaluate[A](className: String, methodName: String, outDir: String)(using Typeable[A]): A = {
    val loader = new java.net.URLClassLoader(
      Array(new java.io.File(outDir).toURI.toURL),
      getClass.getClassLoader
    ) {
      override protected def loadClass(name: String, resolve: Boolean): Class[?] = synchronized {
        val loaded = findLoadedClass(name)
        val cls =
          if loaded != null then loaded
          else {
            try findClass(name)
            catch {
              case _: ClassNotFoundException => super.loadClass(name, false)
            }
          }

        if resolve then resolveClass(cls)
        cls
      }
    }
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
    classpath: String = resolvedClasspath()
  )(using Typeable[A]): ValidatedNec[DomainError, A] = {
    requireTypeable[A]
    val tempDir = Files.createTempDirectory("track-compile").toString
    compileFile(scalaFile, tempDir, classpath) match {
      case Validated.Valid(compiledDir) =>
        Validated
          .catchNonFatal(evaluate[A](className, methodName, compiledDir))
          .leftMap { e =>
            NonEmptyChain.one(
              DomainError.TrackFileParseFailed(
                s"Evaluation failed for file '$scalaFile', class '$className', method '$methodName': ${e.getClass.getSimpleName}: ${e.getMessage}"
              )
            )
          }
      case Validated.Invalid(e) =>
        e.invalid
    }
  }

  inline def compileAndEvaluateFile(scalaFile: java.nio.file.Path): ValidatedNec[DomainError, Track] =
    compileAndEvaluateFile[ValidatedNec[DomainError, Track]](
      scalaFile = scalaFile.toString,
      className = classNameFromFilePath(scalaFile),
      methodName = "playWrapper"
    ).andThen(identity)

}
