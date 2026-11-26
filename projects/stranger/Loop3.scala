import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Loop3 {
  given Channel = Channel3
  given Option[Instrument] = instrument(
    Bank.Default,
    Ethnic.Shamisen
  )

  val loop: Track = loop1.repeat(2)
  lazy val loop1: Track = track(
    timing = time(16).repeat(64),
    notes = (note((r, c2), (r, ds2), (r, f2))
      .repeat(5)
      .repeat(2)
      ++ note(c2, ds2))
      .repeat(2)
      .transpose(24),
    velocity = velocity(40)
  )
}
