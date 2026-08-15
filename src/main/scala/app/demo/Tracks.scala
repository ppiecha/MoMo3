package app.demo

import app.domain.Generator.*
import app.syntax.Conversions.{repeatN, given}
import app.domain.*

object Tracks {

  val repeatCount = 2

  given velocityGen: Option[Generator[Velocity]] = None

  val track1 = Track(
    channel = Channel.from(9),
    timeGen = TimeGen(Seq(8, 8, 4).repeatN(repeatCount)),
    durGen = DurationGen(Seq(8, 8, 8).repeatN(repeatCount)),
    noteGen = NoteGen(Seq(36, 36, 39).repeatN(repeatCount))
  )

  val track2 = Track(
    channel = Channel.from(9),
    timeGen = TimeGen(LazyList(4, 4).repeatN(repeatCount)),
    durGen = DurationGen(LazyList(4, 4).repeatN(repeatCount)),
    noteGen = NoteGen(LazyList(0, 39).repeatN(repeatCount))
  )

  val track3 = Track(
    channel = Channel.from(9),
    timeGen = TimeGen(LazyList(8, 8, 8, 8).repeatN(repeatCount)),
    durGen = DurationGen(LazyList(8, 8, 8, 8).repeatN(repeatCount)),
    noteGen = NoteGen(LazyList(36, 36, 36, 36).repeatN(repeatCount))
  )

  val track4 = Track(
    channel = Channel.from(0),
    timeGen = TimeGen(LazyList(4, 4, 2).repeatN(repeatCount)),
    durGen = DurationGen(LazyList(1, 4d / 3, 2).repeatN(repeatCount)),
    noteGen = NoteGen(LazyList(60, 64, 67).repeatN(repeatCount))
  )
}
