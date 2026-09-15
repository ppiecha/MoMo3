package app.config

import app.domain.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import pureconfig.*

case class TimingConfig(
  bpm: Int = 60,
  ppq: Int = Ppq.DEFAULT_VALUE
) derives ConfigReader

case class EnvironmentConfig(
  timingContext: TimingConfig = TimingConfig(),
  synthConfig: SynthConfig = SynthConfig(),
  midiOutputConfig: MidiOutputConfig = MidiOutputConfig(),
  midi: MidiConfig = MidiConfig(),
  paths: PathsConfig = PathsConfig()
) derives ConfigReader

case class Environment private (
  timingContext: TimingContext,
  input: ConsoleInput = stdInput,
  synthConfig: SynthConfig = SynthConfig(),
  midiOutputConfig: MidiOutputConfig = MidiOutputConfig(),
  midiConfig: MidiConfig = MidiConfig(),
  pathsConfig: PathsConfig = PathsConfig()
)

object Environment {
  def from(
    bpm: Int,
    ppq: Int = Ppq.DEFAULT_VALUE,
    synthConfig: SynthConfig = SynthConfig(),
    midiOutputConfig: MidiOutputConfig = MidiOutputConfig(),
    midiConfig: MidiConfig = MidiConfig(),
    pathsConfig: PathsConfig = PathsConfig(),
    input: ConsoleInput = stdInput
  ): Either[DomainError, Environment] = {
    TimingContext
      .from(ppq, bpm)
      .map(tc => Environment(tc, input, synthConfig, midiOutputConfig, midiConfig, pathsConfig))
      .toEither
      .leftMap(errors => DomainError.ValidationFailed(ValidationError.InvalidConfig(errors.toNonEmptyList)))
  }

  def fromConfig(
    config: EnvironmentConfig,
    input: ConsoleInput = stdInput
  ): Either[DomainError, Environment] = {
    from(
      bpm = config.timingContext.bpm,
      ppq = config.timingContext.ppq,
      synthConfig = config.synthConfig,
      midiOutputConfig = config.midiOutputConfig,
      midiConfig = config.midi,
      pathsConfig = config.paths,
      input = input
    )
  }

  def load(
    source: ConfigSource = ConfigSource.default,
    input: ConsoleInput = stdInput
  ): Either[DomainError, Environment] = {
    source.load[EnvironmentConfig] match {
      case Right(config) => fromConfig(config, input)
      case Left(failures) =>
        Left(
          DomainError.ValidationFailed(
            ValidationError.InvalidConfig(
              NonEmptyList.of(ValidationError.InvalidMessage(failures.prettyPrint()))
            )
          )
        )
    }
  }
}
