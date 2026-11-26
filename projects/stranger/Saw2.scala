import app.dsl.{*, given}

object Saw2 {
  given TrackSet = TrackSet(Channel2, instrument(SynthLead.Lead2Sawtooth))

  lazy val saw: Track = saw1.repeat(2)

  val saw1 = steps(
    1,
    notes = note((g2, c3), (gs1, gs2), (f1, f2), (ds2, gs2)),
    velocity = velocity(35)
  )

  val saw2 = saw1.copy(
    instrument = instrument(1, 18),
    velocity = Some(velocity(100))
  )
}
