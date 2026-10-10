package app.config

import com.sun.jna.Library
import com.sun.jna.Native
import org.jline.terminal.TerminalBuilder

import scala.io.StdIn

trait ConsoleInput {
  def readLine(): String
  def readKey(): Char
}

val stdInput = new ConsoleInput {
  private val isWindows: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("win")

  private lazy val terminal =
    TerminalBuilder
      .builder()
      .system(true)
      .build()

  private object WindowsConsole {
    trait Msvcrt extends Library {
      def _getwch(): Int
    }

    lazy val msvcrt: Msvcrt =
      Native.load("msvcrt", classOf[Msvcrt]).asInstanceOf[Msvcrt]

    def readKey(): Char = {
      val first = msvcrt._getwch()
      // Extended keys emit prefix 0 or 224 and then the actual scan code.
      if first == 0 || first == 224 then msvcrt._getwch().toChar else first.toChar
    }
  }

  override def readLine(): String = StdIn.readLine()

  override def readKey(): Char = {
    if isWindows then {
      try WindowsConsole.readKey()
      catch {
        case _: Throwable =>
          Option(readLine()).flatMap(_.headOption).getOrElse('\n')
      }
    } else {
      val attributes = terminal.enterRawMode()
      try {
        val reader = terminal.reader()
        var code   = reader.read()
        while code == -1 do code = reader.read()
        code.toChar
      } finally {
        terminal.setAttributes(attributes)
      }
    }
  }
}
