import app.dsl.{*, given}

object Guitar1 {
  given TrackSet = TrackSet(Channel1, instrument(Guitar.ElectricGuitarClean))

  lazy val guitar: Track = guitar1.repeat(2)

  val guitar1 = steps(
    4,
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
