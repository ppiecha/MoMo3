import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Enrichment4 {
  given Channel            = Channel4
  given Option[Instrument] = instrument(Bank.Default, Ethnic.Shamisen)

  lazy val enrich: Track = (part1 ++ part2).transpose(12).repeat(4)
  val part2 =
    track(
      timing = time(8, 16, 16, 16, 16, 8).repeat(2),
      notes = note(r, ds3, r, r, r, r, r, c3, r, r, r, r),
      velocity = velocity(50)
    )
  val part1 = part2.copy(
    timing = time(16).repeat(16),
    notes = note(r, ds3, r, r, ds3, r, r, c3, r, r, c3, r, r, r, r, r)
  )
}
