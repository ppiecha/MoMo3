package app.midi

import javax.sound.midi
import javax.sound.midi.MidiSystem
import app.*
import cats.data.Reader
import cats.data.Kleisli
import java.io.File
import javax.sound.midi.Soundbank
import javax.sound.midi.Synthesizer
import javax.sound.midi.Sequencer

object Player {

  def getSoundBank(path: String): App[Soundbank] =
    Kleisli { env =>
      val sfFile = new File(path)
      if sfFile.exists() then {
        val sb = MidiSystem.getSoundbank(sfFile)
        Right(sb)
      } else {
        Left(FileError.FileNotFound(path))
      }
    }

  def prepareMidiSystem(sb: Soundbank): App[(Sequencer, Synthesizer)] =
    Kleisli { env =>
      val sequencer = MidiSystem.getSequencer(false)
      sequencer.open()
      val synth = MidiSystem.getSynthesizer()
      synth.open()
      if sb == null then Left(MidiError.SoundbankObjectNotDefined(sb))
      else if !synth.isSoundbankSupported(sb) then Left(MidiError.SoundbankNotSupported(sb))
      else if !synth.loadAllInstruments(sb) then Left(MidiError.SoundbankLoadFailed(sb))
      else Right((sequencer, synth))
    }

  def playSequence(sequence: midi.Sequence): App[Unit] =
    for {
      env                <- Kleisli.ask[ErrorOr, Env]
      sb                 <- getSoundBank(env.soundFontPath)
      pair               <- prepareMidiSystem(sb)
    } yield {
      val (sequencer, synth) = pair
      // route sequencer -> synth
      sequencer.getTransmitter().setReceiver(synth.getReceiver())
      sequencer.setSequence(sequence)
      sequencer.setTempoInBPM(env.ctx.bpm.value.toFloat)
      sequencer.start()
      // Wait for playback to finish
      while (sequencer.isRunning) {
        Thread.sleep(100)
      }
      sequencer.stop()
      sequencer.close()
      synth.close()
      env.console.println("Playback finished!")
    }
}
