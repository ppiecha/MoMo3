package app.syntax
import app.playback.RepeatPolicy

object Conversions {
  given Conversion[Seq[Int], Seq[Double]] with
    def apply(s: Seq[Int]): Seq[Double] = s.map(_.toDouble)

  extension [A](seq: Seq[A]) {
    def repeat(count: Int): Seq[A] = if count <= 0 then Seq.empty else seq ++ seq.repeat(count - 1)
  }
}
