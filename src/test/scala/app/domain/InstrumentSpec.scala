package app.domain

import munit.FunSuite

class InstrumentSpec extends FunSuite {

  test("default instrument selects bank zero and the acoustic grand piano program") {
    assertEquals(Instrument.Default.bank, Bank.Default)
    assertEquals(Instrument.Default.program, Instrument.Piano.AcousticGrandPiano)
  }

  test("instrument factory preserves the selected bank and program") {
    val instrument = Instrument.instrument(Bank.Percussion, Instrument.PercussionKit.ElectronicKit)

    assertEquals(instrument, Some(Instrument(Bank.Percussion, Instrument.PercussionKit.ElectronicKit)))
  }
}
