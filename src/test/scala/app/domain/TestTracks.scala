package app.domain

import app.config.*
import app.midi.*
import Generator.*

object TestTracks {

  val oneNoteTrack = Track(
    channel = Channel.from(0),
    timeGen = TimeGen(LazyList(4)),
    durGen = DurationGen(LazyList(8)),
    noteGen = NoteGen(LazyList(60))
  )

  val twoNotesTrack = Track(
    channel = Channel.from(0),
    timeGen = TimeGen(LazyList(4, 4)),
    durGen = DurationGen(LazyList(1, 1)),
    noteGen = NoteGen(LazyList(60, 62))
  )

  val threeNotesTrack = Track(
    channel = Channel.from(0),
    timeGen = TimeGen(LazyList(4, 4, 2)),
    durGen = DurationGen(LazyList(1, 4d / 3, 2)),
    noteGen = NoteGen(LazyList(60, 64, 67))
  )

}
