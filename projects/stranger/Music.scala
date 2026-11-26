//package test1

import app.dsl.*
import app.syntax.MusicFile

val verseDuration = 8

object Music extends MusicFile {

//  def play: Section = bass
//    def play: Section = drumsPadBass
//  def play: Section = drumsPadPiano
  def play: Section = rythmLeadPadPianoPianoHigh

  override def bpm = Some(70)

  val bass  = Section(Bass0.bass)
  val bass1 = Section(Bass0.base1)
  val drums = Section(Drums9.drums)
  val lead  = Section(Guitar1.guitar)
  val pad   = Section(Saw2.saw)
  val piano = Section(Loop3.loop)
  val rythm = Section(
    Drums9.drums,
    Bass0.bass
  )
  val drumsPadPiano = Section(
    Drums9.drums,
    Saw2.saw,
    Loop3.loop
  )
  val drumsPadBass = Section(
    Drums9.drums,
    Saw2.saw,
    Bass0.bass,
    Loop3.loop
    // Piano5.piano,
  )
  val rythmLead = Section(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar
  )
  val rythmLeadPad = Section(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar,
    Saw2.saw
  )
  val rythmLeadPadPiano = Section(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar,
    Saw2.saw,
    Loop3.loop
  )
  val rythmLeadPadPianoPianoHigh = Section(
    Drums9.drums,
    Bass0.bass,
    Guitar1.guitar,
    Saw2.saw,
    Loop3.loop,
    Enrichment4.enrich
  )
}
