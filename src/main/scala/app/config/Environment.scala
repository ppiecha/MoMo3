package app.config

import app.domain.*
import cats.data.ValidatedNec
import app.domain.ValidationError
import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

case class Environment private (
  timingContext: TimingContext,
  input: ConsoleInput = stdInput,
  synthConfig: SynthConfig = SynthConfig(),
  midiOutputConfig: MidiOutputConfig = MidiOutputConfig()
  //    logger: Logger[IO] = Slf4jLogger.getLogger[IO]
)

object Environment {
  def from(
    bpm: Int,
    ppq: Int = Ppq.DEFAULT_VALUE,
    input: ConsoleInput = stdInput
  ): ValidatedNec[ValidationError, Environment] = {
    val timingContext = TimingContext.from(ppq, bpm)
    timingContext.map(tc => Environment(tc, input))
  }
}
