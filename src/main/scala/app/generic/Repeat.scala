package app.generic

object Repeat {

  def repeatByName[A](a: => A, count: Int)(using concat: Concatenable[A]): A = {
    if count <= 1 then a
    else concat.++(a, repeatByName(a, count - 1))
  }

}
