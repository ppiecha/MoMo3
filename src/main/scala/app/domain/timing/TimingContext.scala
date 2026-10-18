package app.domain

import app.domain.Bpm
import app.domain.Ppq
import cats.data.ValidatedNec

case class TimingContext private (bpm: Bpm, ppq: Ppq = Ppq.DEFAULT_PPQ)

object TimingContext {
  def from(ppq: Int, bpm: Int): ValidatedNec[DomainError, TimingContext] = {
    Ppq.from(ppq).product(Bpm.from(bpm)).map { case (p, b) => TimingContext(b, p) }
  }
}
