package app.application

import cats.effect.*
import cats.syntax.all.*
import cats.data.Validated.{Invalid, Valid}

import app.config.*
import app.midi.*
import app.domain.*
import javax.sound.midi.MidiEvent

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
          case _            => ValidationError.EmptyListInSlidingWindow.invalidNec[Time]
        }
      )
      .to(LazyList)

  def eventList(track: Track, env: Environment): LazyList[IsValid[Event]] = {
    val time     = accumulateTimes(track, env)
    val note     = Generator.parse(track.noteGen, env)
    val duration = Generator.parse(track.durGen, env)
    time
      .zip(note)
      .zip(duration)
      .map { case ((t, n), d) => (t, n, d).mapN((time, note, duration) => Event(track.channel, time, note, duration)) }
  }

  def compile(track: Track, env: Environment): CompiledTrack = {
    CompiledTrack(eventList(track, env).map {
      case Valid(event)    => Right(event)
      case Invalid(errors) => Left(ValidationError.InvalidEvent(errors.toList.map(_.toString).mkString(", ")))
    })
  }

}
