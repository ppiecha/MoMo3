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
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import scala.compiletime.error
import scala.compiletime.summonFrom
import scala.reflect.Typeable

object TrackFileParser {

  final case class CompilationError(fileName: String, lineNumber: Int, message: String)

  def extractCompilationError(rawDiagnostic: String): CompilationError =
    DiagnosticParser.extractCompilationError(rawDiagnostic)

  private def resolvedClasspath(): String = ClasspathResolver.resolvedClasspath()

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
  ): ValidatedNec[DomainError, String] =
    compileFiles(Seq(scalaFile), outDir, classpath)

  def compileFiles(
    scalaFiles: Seq[String],
    outDir: String,
    classpath: String = resolvedClasspath()
  ): ValidatedNec[DomainError, String] = {

    val reporter = new StoreReporter()

    val args = (scalaFiles.toArray ++ Array(
      "-d",
      outDir,
      "-classpath",
      classpath
    ))

    val driver = new Driver:
      override protected def newCompiler(using Context) =
        new Compiler

    driver.process(args, reporter)

    if reporter.hasErrors then
      NonEmptyChain
        .fromSeq(
          reporter.allErrors.map { error =>
            val parsedError = extractCompilationError(error.toString)
            DomainError.MusicFileParseFailed(
              s"${parsedError.fileName}:${parsedError.lineNumber}: ${parsedError.message}"
            )
          }
        )
        .getOrElse(NonEmptyChain.one(DomainError.MusicFileParseFailed("unknown error")))
        .invalid[String]
    else outDir.validNec[DomainError]
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

  def catchAllNec[A](thunk: => A): ValidatedNec[DomainError, A] =
    try Validated.validNec(thunk)
    catch {
      case t: Throwable =>
        val root = t match
          case invocation: InvocationTargetException if invocation.getCause != null => invocation.getCause
          case other                                                                => other

        val formattedMessage =
          Option(root.getMessage)
            .map(_.trim)
            .filter(_.nonEmpty)
            .map(msg => s"${root.getClass.getSimpleName}: $msg")
            .getOrElse(root.toString)

        Validated.invalidNec(DomainError.MusicFileParseFailed(formattedMessage))
    }

  inline def compileAndEvaluateFile[A](
    scalaFile: String,
    className: String,
    methodName: String,
    sourceFiles: Seq[String] = Seq.empty,
    classpath: String = resolvedClasspath()
  )(using Typeable[A]): ValidatedNec[DomainError, A] = {
    requireTypeable[A]
    val tempDir = Files.createTempDirectory("track-compile").toString
    compileFiles((scalaFile +: sourceFiles).distinct, tempDir, classpath) match {
      case Validated.Valid(compiledDir) =>
        catchAllNec(RuntimeEvaluator.evaluate[A](className, methodName, compiledDir))
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

  private object DiagnosticParser {
    private val DiagnosticErrorPattern =
      "(?s)^class [^ ]+ at (.+?):(?:<[^>]+>|\\[[^\\]]+\\]) L(\\d+):\\s*(.*)$".r

    def extractCompilationError(rawDiagnostic: String): CompilationError = {
      val diagnostic = rawDiagnostic.trim
      diagnostic match
        case DiagnosticErrorPattern(filePath, lineNumber, message) =>
          val normalizedPath = filePath.replace('\\', '/')
          val fileName       = normalizedPath.split('/').lastOption.getOrElse(filePath)
          CompilationError(fileName = fileName, lineNumber = lineNumber.toInt, message = message.trim)
        case _ =>
          CompilationError(fileName = "unknown", lineNumber = -1, message = diagnostic)
    }
  }

  private object ClasspathResolver {
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

    def resolvedClasspath(): String = {
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
        classLocation(classOf[app.syntax.MusicFile]),
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
  }

  private object RuntimeEvaluator {
    def evaluate[A](className: String, methodName: String, outDir: String): A = {
      val loader = new java.net.URLClassLoader(
        Array(new java.io.File(outDir).toURI.toURL),
        TrackFileParser.getClass.getClassLoader
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
  }

}
