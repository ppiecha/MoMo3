package app.domain

import app.domain.{Bpm, Ppq, ValidationError}
import cats.data.ValidatedNec
import cats.syntax.all.*

case class TimingContext private (bpm: Bpm, ppq: Ppq = Ppq.DEFAULT_PPQ)

object TimingContext {
  def from(ppq: Int, bpm: Int): ValidatedNec[ValidationError, TimingContext] = {
    Ppq.from(ppq).product(Bpm.from(bpm)).map { case (p, b) => TimingContext(b, p) }
  }
}
