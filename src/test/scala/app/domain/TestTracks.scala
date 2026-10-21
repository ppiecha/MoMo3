package app.domain

import app.domain.Track._

object TestTracks {

  given Channel = Channel.Channel0

  val oneNoteTrack: Track =
    track(
      time(4),
      note(60),
      duration = Some(duration(8))
    )

  val twoNotesTrack: Track =
    track(
      time(4, 4),
      note(60, 62),
      duration = Some(duration(1, 1))
    )

  val threeNotesTrack: Track =
    track(
      time(4, 4, 2),
      note(60, 64, 67),
      duration = Some(duration(1, 4d / 3, 2))
    )

}
