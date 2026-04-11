package app.model

import app.*
import app.midi.*
import Generator.*

object TestTracks {

  val testEnv = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)

  val oneNoteTrack = Track(
    channel = Channel.unsafe(0),
    TimeGen(LazyList(4)),
    DurationGen(LazyList(8)),
    NoteGen(LazyList(60))
  )

  val twoNotesTrack = Track(
    channel = Channel.unsafe(0),
    TimeGen(LazyList(4, 4)),
    DurationGen(LazyList(1, 1)),
    NoteGen(LazyList(60, 62))
  )  

  val threeNotesTrack = Track(
    channel = Channel.unsafe(0),
    TimeGen(LazyList(4, 4, 2)),
    DurationGen(LazyList(1, 4/3, 2)),
    NoteGen(LazyList(60, 64, 67))
  )

}
