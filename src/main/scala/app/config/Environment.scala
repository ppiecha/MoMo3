package app.config

import app.domain._
import cats.data.NonEmptyList
import cats.syntax.all._
import pureconfig._

case class TimingConfig(
  bpm: Int,
  ppq: Int
) derives ConfigReader

case class EnvironmentConfig(
  timingContext: TimingConfig,
  synthConfig: SynthConfig,
  midiOutputConfig: MidiOutputConfig,
  midi: MidiConfig,
  paths: PathsConfig,
  startPlaybackOnStartup: Boolean
) derives ConfigReader

case class Environment private (
  timingContext: TimingContext,
  input: ConsoleInput,
  synthConfig: SynthConfig,
  midiOutputConfig: MidiOutputConfig,
  midiConfig: MidiConfig,
  pathsConfig: PathsConfig,
  startPlaybackOnStartup: Boolean
)

object Environment {
  def from(
    bpm: Int,
    ppq: Int,
    synthConfig: SynthConfig,
    midiOutputConfig: MidiOutputConfig,
    midiConfig: MidiConfig,
    pathsConfig: PathsConfig,
    startPlaybackOnStartup: Boolean,
    input: ConsoleInput
  ): Either[DomainError, Environment] = {
    TimingContext
      .from(ppq, bpm)
      .map(tc => Environment(tc, input, synthConfig, midiOutputConfig, midiConfig, pathsConfig, startPlaybackOnStartup))
      .toEither
      .leftMap(errors => DomainError.InvalidConfig(errors.toNonEmptyList))
  }

  def fromConfig(
    config: EnvironmentConfig,
    input: ConsoleInput
  ): Either[DomainError, Environment] = {
    from(
      bpm = config.timingContext.bpm,
      ppq = config.timingContext.ppq,
      synthConfig = config.synthConfig,
      midiOutputConfig = config.midiOutputConfig,
      midiConfig = config.midi,
      pathsConfig = config.paths,
      startPlaybackOnStartup = config.startPlaybackOnStartup,
      input = input
    )
  }

  def load(
    source: ConfigSource,
    input: ConsoleInput
  ): Either[DomainError, Environment] = {
    source.load[EnvironmentConfig] match {
      case Right(config) => fromConfig(config, input)
      case Left(failures) =>
        Left(
          DomainError.InvalidConfig(
            NonEmptyList.of(DomainError.InvalidMessage(failures.prettyPrint()))
          )
        )
    }
  }
}
