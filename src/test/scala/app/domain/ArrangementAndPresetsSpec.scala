package app.domain

import munit.FunSuite

class ArrangementAndPresetsSpec extends FunSuite {

  given Channel = Channel.Ch0

  test("pattern repeat multiplies track length") {
    val lead = Track.track(
      Track.time(8, 8),
      Track.duration(8, 8),
      Track.note(60, 62),
      Track.velocity(90, 100)
    )

    val pattern  = Pattern.from("lead", "lead" -> lead)
    val repeated = pattern.repeat(3)

    assertEquals(repeated.tracks.head.timeGen.length, 6)
    assertEquals(repeated.tracks.head.durGen.length, 6)
  }

  test("section render concatenates pattern blocks") {
    val a = Track.track(Track.time(4), Track.duration(4), Track.note(60), Track.velocity(100))
    val b = Track.track(Track.time(8), Track.duration(8), Track.note(62), Track.velocity(90))

    val p1 = Pattern.from("p1", "lead" -> a)
    val p2 = Pattern.from("p2", "lead" -> b)

    val section  = Section.fromBlocks("verse", Vector(p1 -> 1, p2 -> 2))
    val rendered = section.render.tracks.head

    assertEquals(rendered, a ++ b ++ b)
  }

  test("song arrangement concatenates sections") {
    val a = Track.track(Track.time(4), Track.duration(4), Track.note(60), Track.velocity(100))
    val b = Track.track(Track.time(8), Track.duration(8), Track.note(64), Track.velocity(95))

    val intro = Section.single("intro", Pattern.from("intro-pattern", "lead" -> a), repeats = 1)
    val drop  = Section.single("drop", Pattern.from("drop-pattern", "lead" -> b), repeats = 1)

    val arrangement = SongArrangement.fromSections("song", Vector(intro, drop))

    assertEquals(arrangement.tracks.head, a ++ b)
  }

  test("harmonic tools expose basic scale and triad building") {
    val major = HarmonicTools.scale(60, HarmonicTools.ScaleKind.Major)
    val minor = HarmonicTools.triad(60, HarmonicTools.TriadQuality.Minor)

    assertEquals(major, Vector(60, 62, 64, 65, 67, 69, 71))
    assertEquals(minor, (60, 63, 67))
  }

  test("house groove preset provides aligned stem lengths") {
    val pattern = TrackPresets.houseGroovePattern(bars = 2)

    assertEquals(pattern.trackNames, Vector("kick", "hat", "bass"))
    assertEquals(pattern.tracks.map(_.timeGen.length).distinct.size, 1)
    assertEquals(pattern.tracks.map(_.durGen.length).distinct.size, 1)
  }
}
