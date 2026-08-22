package app.playback

import app.config.Environment
import app.domain.{AbsoluteMidiEvent, DomainError, TimingContext, Track}
import app.midi.{ReactiveSynth, toMidiMessages}
import cats.data.EitherT
import cats.effect.IO

trait PlaybackRunner {
  def run(tracks: List[Track], timing: TimingContext): IO[Either[DomainError, Unit]]
}

object PlaybackRunner {
  def live(env: Environment): PlaybackRunner = new PlaybackRunner {
    override def run(tracks: List[Track], timing: TimingContext): IO[Either[DomainError, Unit]] =
      ReactiveSynth
        .outputResource[IO](env.midiOutputConfig)
        .use { sendMidi =>
          val sendEvent: AbsoluteMidiEvent => IO[Unit] = event => sendMidi(event.command.toMidiMessages).value.void
          val player                                   = Player.live(sendEvent)
          EitherT(player.play(tracks, env.timingContext))
        }
        .value
  }
}
