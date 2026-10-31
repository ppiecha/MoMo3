package app.playback

import app.domain.DomainError
import app.domain.Track
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all.*
import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.*

import java.io.File
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.util.Comparator
import scala.compiletime.error
import scala.compiletime.summonFrom
import scala.reflect.Typeable

object TrackFileParser {

  final case class CompilationError(fileName: String, lineNumber: Int, message: String)

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unsafeCast[A](value: AnyRef): A =
    value.asInstanceOf[A]

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def unsafeJavaNull[T]: T = unsafeCast[T](null)

  def extractCompilationError(rawDiagnostic: String): CompilationError =
    DiagnosticParser.parse(rawDiagnostic).compilationError

  def resolvedClasspath(): String = ClasspathResolver.resolvedClasspath()

  def classNameFromFilePath(path: Path): ValidatedNec[DomainError, String] = {
    val fileName = path.getFileName.toString
    if !fileName.endsWith(".scala") then
      Validated.invalidNec(DomainError.MusicFileParseFailed(s"File '$fileName' is not a Scala source file."))
    else
      val dotIndex = fileName.lastIndexOf('.')
      if (dotIndex == -1) Validated.validNec(fileName)
      else Validated.validNec(fileName.substring(0, dotIndex))
  }

  def compileFile(
    scalaFile: String,
    outDir: String,
    classpath: String
  ): ValidatedNec[DomainError, String] =
    compileFiles(Seq(scalaFile), outDir, classpath)

  def compileFiles(
    scalaFiles: Seq[String],
    outDir: String,
    classpath: String
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
            val parsed      = DiagnosticParser.parse(error.toString)
            val parsedError = parsed.compilationError
            val context =
              if parsed.sourcePath == "unknown" then
                scalaFiles
                  .map(sourceContext)
                  .mkString("sources=[", "; ", "]")
              else sourceContext(parsed.sourcePath)
            DomainError.MusicFileParseFailed(
              s"$context, line=${parsedError.lineNumber}: ${parsedError.message}"
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
    className: ValidatedNec[DomainError, String],
    methodName: String,
    sourceFiles: Seq[String],
    classpath: String
  )(using Typeable[A]): ValidatedNec[DomainError, A] = {
    requireTypeable[A]
    val tempDir = Files.createTempDirectory("track-compile")
    try {
      compileFiles((scalaFile +: sourceFiles).distinct, tempDir.toString, classpath) match {
        case Validated.Valid(compiledDir) =>
          className
            .leftMap(_.map(error => addSourceContext(error, scalaFile)))
            .andThen { cn =>
              catchAllNec(RuntimeEvaluator.evaluate[A](cn, methodName, compiledDir))
                .leftMap(_.map(error => addSourceContext(error, scalaFile)))
            }
        case Validated.Invalid(e) =>
          e.invalid
      }
    } finally {
      Cleanup.deleteDirectoryBestEffort(tempDir)
    }
  }

  inline def compileAndEvaluateFile(
    scalaFile: java.nio.file.Path,
    classpath: String
  ): ValidatedNec[DomainError, Track] =
    compileAndEvaluateFile[ValidatedNec[DomainError, Track]](
      scalaFile = scalaFile.toString,
      className = classNameFromFilePath(scalaFile),
      methodName = "playWrapper",
      sourceFiles = Nil,
      classpath = classpath
    ).andThen(identity)

  private object DiagnosticParser {
    final case class Parsed(compilationError: CompilationError, sourcePath: String)

    private val DiagnosticErrorPattern =
      "(?s)^class [^ ]+ at (.+?):(?:<[^>]+>|\\[[^\\]]+\\]) L(\\d+):\\s*(.*)$".r

    def parse(rawDiagnostic: String): Parsed = {
      val diagnostic = rawDiagnostic.trim
      diagnostic match
        case DiagnosticErrorPattern(filePath, lineNumber, message) =>
          val normalizedPath = filePath.replace('\\', '/')
          val fileName       = normalizedPath.split('/').lastOption.getOrElse(filePath)
          Parsed(
            CompilationError(fileName = fileName, lineNumber = lineNumber.toInt, message = message.trim),
            sourcePath = filePath
          )
        case _ =>
          Parsed(
            CompilationError(fileName = "unknown", lineNumber = -1, message = diagnostic),
            sourcePath = "unknown"
          )
    }
  }

  private def sourceContext(sourcePath: String): String = s"source='$sourcePath'"

  private def addSourceContext(error: DomainError, sourcePath: String): DomainError =
    error match {
      case DomainError.MusicFileParseFailed(message) =>
        DomainError.MusicFileParseFailed(s"${sourceContext(sourcePath)}: $message")
      case other => other
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
      try {
        val cls    = loader.loadClass(className + "$")
        val module = cls.getField("MODULE$").get(unsafeJavaNull[AnyRef])
        val method = cls.getMethod(methodName)
        val result = method.invoke(module)
        unsafeCast[A](result)
      } finally {
        loader.close()
      }
    }
  }

  private object Cleanup {
    def deleteDirectoryBestEffort(directory: Path): Unit =
      try deleteDirectory(directory)
      catch {
        case _: Throwable => ()
      }

    private def deleteDirectory(directory: Path): Unit =
      if Files.exists(directory) then {
        val paths = Files.walk(directory)
        try {
          paths
            .sorted(Comparator.reverseOrder())
            .forEach(path => Files.deleteIfExists(path))
        } finally {
          paths.close()
        }
      }
  }

}
