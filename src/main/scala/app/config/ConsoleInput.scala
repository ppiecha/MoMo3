package app.config

import scala.io.StdIn

trait ConsoleInput {
  def readLine(): String
}

val stdInput = new ConsoleInput {
  override def readLine(): String = StdIn.readLine()
}
