# MoMo3

MoMo3 is a MIDI sequencer where music is written in a Scala DSL. Track files are compiled dynamically in the background when they change, so playback can be updated without restarting the application.

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
