package app.config

import app.domain.{Bpm, Ppq}
import app.shared.IsValid
import cats.syntax.all.*

case class TimingContext private (bpm: Bpm, ppq: Ppq = Ppq.DEFAULT_PPQ)

object TimingContext {
  def from(ppq: Int, bpm: Int): IsValid[TimingContext] = {
    Ppq.from(ppq).product(Bpm.from(bpm)).map { case (p, b) => TimingContext(b, p) }
  }
}
