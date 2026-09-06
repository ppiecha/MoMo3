package app.syntax
import app.playback.RepeatPolicy

object Conversions {
  given Conversion[Seq[Int], Seq[Double]] with
    def apply(s: Seq[Int]): Seq[Double] = s.map(_.toDouble)

  extension [A](ll: Seq[A]) {
    def repeat(policy: RepeatPolicy): Seq[A] = if policy.remaining <= 0 then Seq.empty else ll ++ ll.repeat(policy.next)
  }
}
