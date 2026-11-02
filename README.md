# MoMo3

MoMo3 is a MIDI sequencer where music is written in Scala DSL. \
Track files are compiled dynamically in the background when they change, so playback can be updated without restarting the application.

## Requirements

- Java 17
- Scala 3.3.7
- sbt
- A MIDI output device where you can send MIDI messages. You can use a virtual MIDI port like loopMIDI on Windows or IAC Driver on macOS.
- Listener for the MIDI output device (e.g. FluidSynth, CakeWalk Sonar, GarageBand, Ableton Live, etc.)

## Commands

- Compile: `sbt compile`
- Run: `sbt run`
- Test: `sbt test`

## Track files

Notes can be specified as MIDI numbers (0-127) or by pitch constants from `Note.a0` through `Note.c8`. Sharps use `s` (for example `Note.cs2`); names without an octave such as `Note.c` and `Note.cs` alias octave 4.

```scala
Track.note(Note.a0, Note.as0, Note.b0, Note.c1, Note.cs1, Note.c8)
```
