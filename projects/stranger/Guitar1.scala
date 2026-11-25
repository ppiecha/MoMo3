import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Guitar1 {
  given Channel = Channel1
  given Option[Instrument] = instrument(
    Bank.Default,
    Guitar.ElectricGuitarClean
  )

  lazy val guitar: Track = guitar1.repeat(2)

  val guitar1 = track(
    timing = time(4).repeat(16),
    notes = note(
      (c3, g4),
      ds3,
      f3,
      g3,
      (gs3, gs4, ds4),
      c3,
      ds3,
      f3,
      (f4, c4),
      ds3,
      f3,
      g3,
      (gs3, gs4),
      c3,
      ds3,
      f3
    ).transpose(-12),
    velocity = velocity(30)
  )
}
