import app.dsl.{*, given}

object Pip5 {
  given TrackSet = TrackSet(Channel5, instrument(85))

  lazy val pip = pip1.repeat(4)

  val pip1 = pause(1) ++
    steps(
      16,
      notes = note(r, ds3, r, r, ds3, r, r, c3, r, r, c3, r, r, r, r, r).transpose(24),
      velocity = velocity(50),
      muted = true
    )
}
