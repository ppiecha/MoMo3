package app.demo

import app.domain.Generator.*
import app.syntax.Conversions.{repeat, given}
import app.domain.*
import app.playback.RepeatPolicy.*

object Tracks {

  val repeatCount = 2

  val track1 = Track(
    channel = Channel.from(9),
    timeGen = TimeGen(Seq(8, 8, 4).repeat(fixed(repeatCount))),
    durGen = DurationGen(Seq(8, 8, 8).repeat(fixed(repeatCount))),
    noteGen = NoteGen(Seq(36, 36, 39).repeat(fixed(repeatCount)))
  )

  val track2 = Track(
    channel = Channel.from(9),
    timeGen = TimeGen(LazyList(4, 4).repeat(fixed(repeatCount))),
    durGen = DurationGen(LazyList(4, 4).repeat(fixed(repeatCount))),
    noteGen = NoteGen(LazyList(0, 39).repeat(fixed(repeatCount)))
  )

  val track3 = Track(
    channel = Channel.from(9),
    timeGen = TimeGen(LazyList(8, 8, 8, 8).repeat(fixed(repeatCount))),
    durGen = DurationGen(LazyList(8, 8, 8, 8).repeat(fixed(repeatCount))),
    noteGen = NoteGen(LazyList(36, 36, 36, 36).repeat(fixed(repeatCount)))
  )

  val track4 = Track(
    channel = Channel.from(0),
    timeGen = TimeGen(LazyList(4, 4, 2).repeat(fixed(repeatCount))),
    durGen = DurationGen(LazyList(1, 4d / 3, 2).repeat(fixed(repeatCount))),
    noteGen = NoteGen(LazyList(60, 64, 67).repeat(fixed(repeatCount)))
  )
}
