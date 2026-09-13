package app.demo

import app.domain.Generator.*
import app.syntax.Conversions.{repeat, given}
import app.domain.*
import app.domain.Track.*
import app.playback.RepeatPolicy.*

object Tracks {

  val repeatCount = 2
  given Channel = Channel.Ch0

  val track1: Track = track(
    time(8, 8, 4).repeat(repeatCount),
    duration(8, 8, 8).repeat(repeatCount),
    note(36, 36, 39).repeat(repeatCount)
  )

  val track2: Track = track(
    time(4, 4).repeat(repeatCount),
    duration(4, 4).repeat(repeatCount),
    note(0, 39).repeat(repeatCount)
  )

  val track3: Track = track(
    time(8, 8, 8, 8).repeat(repeatCount),
    duration(8, 8, 8, 8).repeat(repeatCount),
    note(36, 36, 36, 36).repeat(repeatCount)
  )(using Channel.Ch9)

  val track4: Track = track(
    time(4, 4, 2).repeat(repeatCount),
    duration(1, 4d / 3, 2).repeat(repeatCount),
    note(60, 64, 67).repeat(repeatCount)
  )
}