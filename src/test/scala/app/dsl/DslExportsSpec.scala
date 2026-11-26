package app.dsl

import app.dsl.{*, given}
import munit.FunSuite

class DslExportsSpec extends FunSuite {
  test("the DSL exports instrument families and track builders") {
    val programs = List(
      Piano.AcousticGrandPiano,
      ChromaticPercussion.Celesta,
      Organ.DrawbarOrgan,
      Guitar.AcousticGuitarNylon,
      Bass.AcousticBass,
      Strings.Violin,
      Ensemble.StringEnsemble1,
      Brass.Trumpet,
      Reed.SopranoSax,
      Pipe.Piccolo,
      SynthLead.Lead1Square,
      SynthPad.Pad1NewAge,
      SynthEffects.Fx1Rain,
      Ethnic.Sitar,
      Percussive.TinkleBell,
      SoundEffects.GuitarFretNoise,
      PercussionKit.ElectronicKit,
      PercussionNote.BassDrum.BassDrum1
    )
    assertEquals(programs.length, 18)
    assertEquals(Drum.bd, bd)
    assertEquals(instrument(20), Some(Instrument(Bank.Default, 20)))
    assertEquals(instrument(1, 20), Some(Instrument(1, 20)))
    assertEquals(Default, Instrument.Default)

    given TrackSet = TrackSet(Channel0, instrument(20))
    val explicit   = track(time(4), note(c1), duration = duration(4), velocity = velocity(70))
    val derived    = steps(4, note(c1))
    assertEquals(explicit.timing, derived.timing)
    assertEquals(pause(4).timing, derived.timing)
  }
}
