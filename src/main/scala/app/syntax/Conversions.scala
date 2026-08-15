package app.syntax

object Conversions {
  given Conversion[Seq[Int], Seq[Double]] with
    def apply(s: Seq[Int]): Seq[Double] = s.map(_.toDouble)

  extension [A](ll: Seq[A]) {
    def repeatN(n: Int): Seq[A] = if n <= 0 then Seq.empty else ll ++ ll.repeatN(n - 1)
  }
}
