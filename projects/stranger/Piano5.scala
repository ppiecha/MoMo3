import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Piano5 {
  given Channel            = Channel5
  given Option[Instrument] = instrument(Bank.Default, Strings.OrchestralHarp)

  val piano: Track = track(
    timing = time(8).repeat(64),
    notes = note(
      c4,
      ds4,
      g4,
      c5,
      g4,
      ds4,
      g4,
      ds4,
      gs3,
      c4,
      ds4,
      gs4,
      ds4,
      c4,
      ds4,
      c4,
      f3,
      gs3,
      c4,
      f4,
      c4,
      gs3,
      c4,
      gs3,
      gs3,
      c4,
      ds4,
      gs4,
      c5,
      gs4,
      ds4,
      c4,
      c4,
      g4,
      ds4,
      c5,
      ds5,
      c5,
      g4,
      ds4,
      gs3,
      ds4,
      c4,
      gs4,
      c5,
      gs4,
      ds4,
      c4,
      f3,
      c4,
      gs3,
      f4,
      gs4,
      f4,
      c4,
      gs3,
      gs3,
      c4,
      ds4,
      gs4,
      ds4,
      c4,
      ds4,
      c4
    ).transpose(0),
    velocity = velocity(50).repeat(8)
  )
}
