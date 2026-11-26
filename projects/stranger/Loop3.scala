import app.dsl.{*, given}

object Loop3 {
  given TrackSet = TrackSet(Channel3, instrument(Ethnic.Shamisen))

  val loop: Track = loop1.repeat(2)

  lazy val loop1: Track = steps(
    16,
    notes = (note((r, c2), (r, ds2), (r, f2)).repeat(10) ++ note(c2, ds2))
      .repeat(2)
      .transpose(24),
    velocity = velocity(40)
  )
}
