package app.application

import cats.effect.*
import cats.syntax.all.*
import cats.data.Validated.{Invalid, Valid}

import app.*
import app.midi.*
import app.domain.*
import javax.sound.midi.MidiEvent

case class CompiledTrack(
    events: LazyList[Event],
    midiEvents: LazyList[MidiEvent]
)

object TrackCompiler {

  def accumulateTimes(track: Track, env: Environment): LazyList[IsValid[Time]] =
    Generator
      .parse(track.timeGen, env)
      .scan(Time.zero.validNec[ValidationError])((acc, time) =>
        (acc, time).mapN((a, t) => Time(t.duration, a.tick + t.tick))
      )
      .sliding(2)
      .map(l2 =>
        l2.toList match {
          case List(t1, t2) => (t1, t2).mapN((curr, next) => Time(next.duration, curr.tick))
          case List(t2)     => t2
          case _            => throw new RuntimeException("Empty list in sliding window, this should never happen")
        }
      )
      .to(LazyList)

  def eventList(track: Track, env: Environment): LazyList[Event] = {
    val time     = accumulateTimes(track, env)
    val note     = Generator.parse(track.noteGen, env)
    val duration = Generator.parse(track.durGen, env)
    time
      .zip(note)
      .zip(duration)
      .flatMap { case ((t, n), d) =>
        (t, n, d)
          .mapN((time, note, duration) => Event.makeList(track.channel, time, note, duration)) match {
          case Valid(events) => events
          case Invalid(nec)  => throw new RuntimeException("Invalid MIDI message: " + nec.toList.mkString(", "))
        }
      }
  }

  def compile(track: Track, env: Environment): CompiledTrack = {
    val events     = eventList(track, env)
    val midiEvents = events.flatMap(_.listOfMidiEvents)
    CompiledTrack(events, midiEvents)
  }

}
