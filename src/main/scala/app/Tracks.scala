import app.model.Generator.*
import app.model.given
import app.midi.*
import app.model.*

object Tracks {

  val repeatCount = 2

  val track1 = Track(
    channel = Channel.unsafe(9),
    TimeGen(LazyList(8, 8, 4).repeatN(repeatCount)),
    DurationGen(LazyList(8, 8, 8).repeatN(repeatCount)),
    NoteGen(LazyList(36, 36, 39).repeatN(repeatCount))
  )

  val track2 = Track(
    channel = Channel.unsafe(9),
    TimeGen(LazyList(4, 4).repeatN(repeatCount)),
    DurationGen(LazyList(4, 4).repeatN(repeatCount)),
    NoteGen(LazyList(0, 39).repeatN(repeatCount))
  )

  val track3 = Track(
    channel = Channel.unsafe(9),
    TimeGen(LazyList(8, 8, 8, 8).repeatN(repeatCount)),
    DurationGen(LazyList(8, 8, 8, 8).repeatN(repeatCount)),
    NoteGen(LazyList(36, 36, 36, 36).repeatN(repeatCount))
  )

  val track4 = Track(
    channel = Channel.unsafe(0),
    TimeGen(LazyList(4, 4, 2).repeatN(repeatCount)),
    DurationGen(LazyList(1, 4/3, 2).repeatN(repeatCount)),
    NoteGen(LazyList(60, 64, 67).repeatN(repeatCount))
  )
}
