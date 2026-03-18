package app.model

import munit.CatsEffectSuite
import Generator.*
import app.midi.*
import app.*
import javax.sound.midi.ShortMessage
import javax.sound.midi.ShortMessage.{CONTROL_CHANGE, PROGRAM_CHANGE, NOTE_OFF, NOTE_ON}
import cats.effect.*

class TrackSpec extends CatsEffectSuite  {

  val testEnv = Env(bpm = Bpm.unsafe(60), consoleImpl, soundFontPath = "sf/soundfont.sf2")
  
  val oneNoteTrack = Track(
      channel = Channel.unsafe(0),
      TimeGen(LazyList(4)),
      DurationGen(LazyList(4)),
      NoteGen(LazyList(60))
    )

  test("One note track should produce NoteOn and NoteOff message") {
    oneNoteTrack.midiStream[IO].value.run(testEnv).flatMap {
      case Right(stream) => stream.compile.toList
      case Left(domainError)     => IO.raiseError(new RuntimeException(domainError.toString))
    }.map { messages =>
      assertEquals(messages.size, 2)
      val List(noteOn, noteOff) = messages.map(_.asInstanceOf[ShortMessage])
      assertEquals(noteOn.getCommand, NOTE_ON)
      assertEquals(noteOn.getData1, 60)
      assertEquals(noteOff.getCommand, NOTE_OFF)
      assertEquals(noteOff.getData1, 60)
    }
  }
  
}
