package app.domain

import app.domain.Track.*

object TestTracks {
  
  given Channel = Channel.Ch0

  val oneNoteTrack: Track =
    track(
      time(4),
      duration(8),
      note(60)
    )

  val twoNotesTrack: Track =
    track(
      time(4, 4),
      duration(1, 1),
      note(60, 62)
    )

  val threeNotesTrack: Track =
    track(
      time(4, 4, 2),
      duration(1, 4d / 3, 2),
      note(60, 64, 67)
    )

}
