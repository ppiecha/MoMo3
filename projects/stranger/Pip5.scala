import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Pip5 {
  given Channel = Channel5
  given Option[Instrument] = instrument(
    0,
    85
  )

  lazy val pip = pip1.repeat(4)
  val pip1 = Track.rest(1) ++
    track(
      timing = time(16).repeat(16),
      notes = note(r, ds3, r, r, ds3, r, r, c3, r, r, c3, r, r, r, r, r).transpose(24),
      velocity = velocity(50),
      isActive = false
    )

}
