import app.dsl.{*, given}

object Drums9 {
  given TrackSet = TrackSet(Channel9, instrument(PercussionKit.ElectronicKit))

  lazy val drums = drums2.repeat(verseDuration)

  val drums2 = steps(
    8,
    notes = note((hhc, bd), hhp) ++
      note((hhc, clap), hhp) ++
      note(hhp, (hhc, bd)) ++
      note((hhc, clap), hhp),
    velocity = velocity(60)
  )
}
