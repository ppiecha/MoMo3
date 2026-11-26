import app.dsl.{*, given}

object Enrichment4 {
  given TrackSet = TrackSet(Channel4, instrument(Ethnic.Shamisen))

  lazy val enrich: Track = (part1 ++ part2).repeat(4)

  val part1 = steps(
    16,
    notes = note(r, ds3, r, r, ds3, r, r, c3, r, r, c3, r, r, r, r, r),
    velocity = velocity(45)
  )

  val part2 = track(
    timing = time(8, 16, 16, 16, 16, 8).repeat(2),
    notes = note(r, ds3, r, r, r, r, r, c3, r, r, r, r),
    velocity = velocity(45)
  )
}
