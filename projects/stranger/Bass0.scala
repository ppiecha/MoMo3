import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.domain.Track.note
import app.syntax.Extensions.*
import app.typeclasses.Rand.*
import app.typeclasses.Repeat.repeatByName
import app.typeclasses.Transposable.given
import cats.data.NonEmptyList

import scala.language.implicitConversions

object Bass0 {
  given Channel = Channel0
  given Option[Instrument] = instrument(
    1,
    20
  )

  lazy val bass: Track = rand(NonEmptyList.of(base1, base2), 2)

  val base1 = track(
    timing = time(16).repeat(64),
    notes = (note(c1).repeat(15) ++
      repeatByName(note(gs0).repeat(3) ++
      rand(note(gs0), note(gs1)), 4) ++
      note(f0).repeat(16) ++
      note(gs0).repeat(15) ++
      note(as0).repeat(1) ++
      note(c1))
      .transpose(12),
    velocity = velocity(70)
  )

  val base2 = base1
    .copy(
      notes = note(c1).repeat(15) ++
        note(gs1).repeat(16) ++
        note(f1).repeat(16) ++
        note(gs1).repeat(15) ++
        note(as0).repeat(1) ++
        note(c1)
    )
}
