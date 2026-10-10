package app.domain

final case class StemTrack(name: String, track: Track)

final case class Pattern private (name: String, stems: Vector[StemTrack]) {
  require(stems.nonEmpty, "Pattern must contain at least one stem")

  def trackNames: Vector[String] = stems.map(_.name)

  def tracks: Vector[Track] = stems.map(_.track)

  def repeat(times: Int): Pattern = {
    require(times > 0, "Pattern repeat count must be positive")
    if times == 1 then this
    else {
      val repeated = stems.map(stem => stem.copy(track = Pattern.repeatTrack(stem.track, times)))
      copy(stems = repeated)
    }
  }

  def withVelocityScale(scale: Double): Pattern = {
    require(scale >= 0.0, "Velocity scale must be non-negative")
    copy(stems = stems.map(stem => stem.copy(track = Pattern.scaleVelocity(stem.track, scale))))
  }
}

object Pattern {
  def from(name: String, stems: (String, Track)*): Pattern = {
    require(stems.nonEmpty, "Pattern must contain at least one stem")
    val names = stems.map(_._1)
    require(names.distinct.size == names.size, "Stem names in a pattern must be unique")
    Pattern(name, stems.toVector.map((name, track) => StemTrack(name, track)))
  }

  private[domain] def repeatTrack(track: Track, times: Int): Track =
    (2 to times).foldLeft(track)((acc, _) => acc ++ track)

  private[domain] def scaleVelocity(track: Track, scale: Double): Track = {
    val scaledVelocity = track.velGen match {
      case Generator.VelocityGen(values) =>
        Generator.VelocityGen(values.map(value => clampVelocity(math.round(value * scale).toInt)))
    }
    Track.track(
      timeGen = track.timeGen,
      durGen = track.durGen,
      noteGen = track.noteGen,
      velGen = scaledVelocity
    )(using track.channel)
  }

  private def clampVelocity(value: Int): Int = value.max(0).min(127)
}

final case class Section private (name: String, blocks: Vector[(Pattern, Int)]) {
  require(blocks.nonEmpty, "Section must contain at least one block")

  private val expectedTrackNames = blocks.head._1.trackNames

  blocks.foreach { case (pattern, repeats) =>
    require(repeats > 0, "Section block repeat count must be positive")
    require(
      pattern.trackNames == expectedTrackNames,
      s"All patterns in section '$name' must expose identical stem names and order"
    )
  }

  def trackNames: Vector[String] = expectedTrackNames

  def render: Pattern = {
    val renderedStems = expectedTrackNames.map { trackName =>
      val concatenated = blocks
        .map { case (pattern, repeats) =>
          val repeated = pattern.repeat(repeats)
          repeated.stems.find(_.name == trackName).get.track
        }
        .reduce(_ ++ _)

      trackName -> concatenated
    }

    Pattern.from(s"$name-rendered", renderedStems*)
  }
}

object Section {
  def single(name: String, pattern: Pattern, repeats: Int = 1): Section =
    fromBlocks(name, Vector(pattern -> repeats))

  def fromBlocks(name: String, blocks: Vector[(Pattern, Int)]): Section =
    Section(name, blocks)
}

final case class SongArrangement private (name: String, sections: Vector[Section]) {
  require(sections.nonEmpty, "Song arrangement must contain at least one section")

  private val expectedTrackNames = sections.head.trackNames

  sections.foreach { section =>
    require(
      section.trackNames == expectedTrackNames,
      s"All sections in arrangement '$name' must expose identical stem names and order"
    )
  }

  def render: Pattern = {
    val renderedStems = expectedTrackNames.map { trackName =>
      val concatenated = sections
        .map(section => section.render.stems.find(_.name == trackName).get.track)
        .reduce(_ ++ _)

      trackName -> concatenated
    }

    Pattern.from(s"$name-rendered", renderedStems*)
  }

  def tracks: Seq[Track] = render.tracks
}

object SongArrangement {
  def fromSections(name: String, sections: Vector[Section]): SongArrangement =
    SongArrangement(name, sections)
}
