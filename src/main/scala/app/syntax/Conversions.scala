package app.syntax

object Conversions {
  given Conversion[Seq[Int], Seq[Double]] with
    def apply(s: Seq[Int]): Seq[Double] = s.map(_.toDouble)
}
