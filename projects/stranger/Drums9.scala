import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Drums9 {
  given Channel = Channel9
  given Option[Instrument] = instrument(
    bank = Bank.Default,
    program = PercussionKit.Tr808Kit
  )

  lazy val drums = drums2.repeat(verseDuration)

  val drums1 = track(
    timing = time(2, 2),
    notes = note(PercussionNote.BassDrum.BassDrum1).repeat(2),
    velocity = velocity(80)
  )

  val drums2 = track(
    timing = time(8).repeat(8),
    notes = note(
      (PercussionNote.HiHat.ClosedHiHat, PercussionNote.BassDrum.BassDrum1),
      PercussionNote.HiHat.PedalHiHat
    ) ++
      note(
        (PercussionNote.HiHat.ClosedHiHat, PercussionNote.Snare.HandClap),
        PercussionNote.HiHat.PedalHiHat
      ) ++
      note(
        PercussionNote.HiHat.PedalHiHat,
        (PercussionNote.HiHat.ClosedHiHat, PercussionNote.BassDrum.BassDrum1)
      ) ++
      note(
        (PercussionNote.HiHat.ClosedHiHat, PercussionNote.Snare.HandClap),
        PercussionNote.HiHat.PedalHiHat
      ),
    velocity = velocity(60)
  )(using
    Channel9,
    instrument(
      Bank.Default,
      PercussionKit.ElectronicKit
    )
  )

}
