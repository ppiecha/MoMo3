package app.config

import app.domain.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import pureconfig.*

case class TimingConfig(
  bpm: Int,
  ppq: Int
) derives ConfigReader

case class EnvironmentConfig(
  timingContext: TimingConfig,
  midiOutputConfig: MidiOutputConfig,
  paths: PathsConfig,
  startPlaybackOnStartup: Boolean
) derives ConfigReader

case class Environment private (
  timingContext: TimingContext,
  input: ConsoleInput,
  midiOutputConfig: MidiOutputConfig,
  pathsConfig: PathsConfig,
  startPlaybackOnStartup: Boolean
)

object Environment {
  def fromConfig(
    config: EnvironmentConfig,
    input: ConsoleInput
  ): Either[DomainError, Environment] = {
    TimingContext
      .from(config.timingContext.ppq, config.timingContext.bpm)
      .map(tc => Environment(tc, input, config.midiOutputConfig, config.paths, config.startPlaybackOnStartup))
      .toEither
      .leftMap(errors => DomainError.InvalidConfig(errors.toNonEmptyList))
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
