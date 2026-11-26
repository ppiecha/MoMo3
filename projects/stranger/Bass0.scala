import app.dsl.{*, given}
import cats.data.NonEmptyList

object Bass0 {
  given TrackSet = TrackSet(Channel0, instrument(1, 20))

  lazy val bass: Track = rand(NonEmptyList.of(base1, base2), 2)

  val base1 = steps(
    16,
    notes = (c1 * 15 ++
      repeatByName(gs0 * 3 ++ rand(note(gs0), note(gs1)), 4) ++
      f0 * 16 ++
      gs0 * 15 ++
      as0 * 1 ++
      c1 * 1)
      .transpose(12),
    velocity = velocity(60)
  )

  val base2 = base1.copy(
    notes = Some(
      c1 * 15 ++
        gs1 * 16 ++
        f1 * 16 ++
        gs1 * 15 ++
        as0 * 1 ++
        c1 * 1
    )
  )
}
