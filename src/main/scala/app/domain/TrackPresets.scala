package app.domain

object TrackPresets {

  private def requirePositiveBars(bars: Int): Int = {
    require(bars > 0, "Bars must be positive")
    bars
  }

  private def repeatToLength[A](seed: Vector[A], length: Int): Vector[A] = {
    require(seed.nonEmpty, "Seed cannot be empty")
    Vector.tabulate(length)(index => seed(index % seed.length))
  }

  def fourOnTheFloorKick(
    bars: Int = 1,
    note: Int = 36,
    velocity: Int = 118
  )(using Channel): Track = {
    val steps      = requirePositiveBars(bars) * 8
    val time       = RhythmTools.constant(steps, 8.0)
    val velocities = repeatToLength(Vector(velocity, 0), steps)

    Track.track(
      time,
      RhythmTools.durationsLike(time),
      Track.note(Vector.fill(steps)(NoteArg.Single(note))*),
      Track.velocity(velocities*)
    )
  }

  def offBeatHiHat(
    bars: Int = 1,
    note: Int = 42
  )(using Channel): Track = {
    val steps = requirePositiveBars(bars) * 8
    val time  = RhythmTools.constant(steps, 8.0)

    Track.track(
      time,
      RhythmTools.durationsLike(time),
      Track.note(Vector.fill(steps)(NoteArg.Single(note))*),
      RhythmTools.pulseVelocity(steps, strong = 88, weak = 70)
    )
  }

  def minorPentatonicBassline(
    root: Int = 36,
    bars: Int = 1
  )(using Channel): Track = {
    val steps   = requirePositiveBars(bars) * 8
    val scale   = HarmonicTools.scale(root, HarmonicTools.ScaleKind.MinorPentatonic)
    val contour = Vector(scale(0), scale(2), scale(3), scale(1), scale(0), scale(2), scale(4), scale(1))
    val notes   = repeatToLength(contour, steps)
    val time    = RhythmTools.swingEighths(steps)

    Track.track(
      time,
      RhythmTools.durationsLike(time),
      Track.note(notes.map(NoteArg.Single.apply)*),
      RhythmTools.pulseVelocity(steps, strong = 104, weak = 86)
    )
  }

  def houseGroovePattern(bars: Int = 1): Pattern = {
    val kick = {
      given Channel = Channel.Ch9
      fourOnTheFloorKick(bars = bars, note = 36)
    }

    val hat = {
      given Channel = Channel.Ch9
      offBeatHiHat(bars = bars, note = 42)
    }

    val bass = {
      given Channel = Channel.Ch1
      minorPentatonicBassline(root = 36, bars = bars)
    }

    Pattern.from(
      name = "house-groove",
      "kick" -> kick,
      "hat"  -> hat,
      "bass" -> bass
    )
  }
}
