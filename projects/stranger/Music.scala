//package test1

import app.domain.*
import app.syntax.MusicFile

val verseDuration = 8

object Music extends MusicFile {

//  def play: Section = bass
//    def play: Section = drumsPadBass
//  def play: Section = drumsPadPiano
  def play: Section = rythmLeadPadPianoPianoHigh

  override def bpm = Some(70)

  val bass  = Section.of(Bass0.bass)
  val bass1 = Section.of(Bass0.base1)
  val drums = Section.of(Drums9.drums)
  val lead  = Section.of(Guitar1.guitar)
  val pad   = Section.of(Saw2.saw)
  val piano = Section.of(Loop3.loop)
  val rythm = Section.of(
    Drums9.drums,
    Bass0.bass
  )
  val drumsPadPiano = Section.of(
    Drums9.drums,
    Saw2.saw,
    Loop3.loop
  )
  val drumsPadBass = Section.of(
    Drums9.drums,
    Saw2.saw,
    Bass0.bass,
    Loop3.loop
    // Piano5.piano,
  )
  val rythmLead = Section.of(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar
  )
  val rythmLeadPad = Section.of(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar,
    Saw2.saw
  )
  val rythmLeadPadPiano = Section.of(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar,
    Saw2.saw,
    Loop3.loop
  )
  val rythmLeadPadPianoPianoHigh = Section.of(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar,
    Saw2.saw,
    Loop3.loop,
    Enrichment4.enrich
  )
}
