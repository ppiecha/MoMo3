//package test1

import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Saw2 {
  given Channel = Channel2
  given Option[Instrument] = instrument(
    Bank.Default,
    SynthLead.Lead2Sawtooth
  )

  lazy val saw: Track = saw1.repeat(2)
  val saw1 = track(
    timing = time(1).repeat(3) ++ time(1),
    notes = note((g2, c3), (gs1, gs2), (f1, f2), (ds2, gs2))
      .transpose(0),
    velocity = velocity(35)
  )
  val saw2 = saw1
    .copy(
      instrument = instrument(1, 18),
      velocity = velocity(100)
    )
    .transpose(0)
}
