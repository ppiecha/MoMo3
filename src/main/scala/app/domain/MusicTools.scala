package app.domain

import cats.data.NonEmptyList

object HarmonicTools {

  enum ScaleKind(val intervals: Vector[Int]) {
    case Major           extends ScaleKind(Vector(0, 2, 4, 5, 7, 9, 11))
    case NaturalMinor    extends ScaleKind(Vector(0, 2, 3, 5, 7, 8, 10))
    case MinorPentatonic extends ScaleKind(Vector(0, 3, 5, 7, 10))
  }

  enum TriadQuality(val intervals: (Int, Int, Int)) {
    case Major      extends TriadQuality((0, 4, 7))
    case Minor      extends TriadQuality((0, 3, 7))
    case Diminished extends TriadQuality((0, 3, 6))
  }

  def scale(root: Int, kind: ScaleKind, octaves: Int = 1): Seq[Int] = {
    require(octaves > 0, "Octaves must be positive")

    (for {
      octave   <- 0 until octaves
      interval <- kind.intervals
    } yield root + interval + octave * 12).toVector
  }

  def triad(root: Int, quality: TriadQuality): (Int, Int, Int) = {
    val (i1, i2, i3) = quality.intervals
    (root + i1, root + i2, root + i3)
  }

  def chordGenerator(chords: Seq[(Int, Int, Int)]): Generator[Note] =
    Generator.NoteGen(
      chords.map { case (a, b, c) =>
        Chord(NonEmptyList(a, List(b, c)))
      }
    )
}

object RhythmTools {

  def constant(steps: Int, value: Double): TimeGen = {
    require(steps > 0, "Rhythm steps must be positive")
    TimeGen(Vector.fill(steps)(value))
  }

  def swingEighths(steps: Int, longValue: Double = 6.0, shortValue: Double = 12.0): TimeGen = {
    require(steps > 0, "Rhythm steps must be positive")
    TimeGen((0 until steps).map(index => if index % 2 == 0 then longValue else shortValue).toVector)
  }

  def durationsLike(time: TimeGen): DurationGen = DurationGen(time.values)

  def pulseVelocity(steps: Int, strong: Int = 110, weak: Int = 90): Generator[Velocity] = {
    require(steps > 0, "Velocity steps must be positive")
    Generator.velocity((0 until steps).map(index => if index % 2 == 0 then strong else weak)*)
  }
}
