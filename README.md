# MoMo3

This project now uses Scala CLI instead of sbt.

## Requirements

- Scala CLI 1.12.x or newer
- A MIDI output device or FluidSynth setup if you want to run the app end-to-end

## Commands

- Compile: `scala-cli compile .`
- Run: `scala-cli run .`
- Test: `scala-cli test .`
- REPL: `scala-cli repl .`

## FluidSynth on Windows

Run loopMIDI first, then start FluidSynth from `C:\tools\fluidsynth\bin`:

```powershell
fluidsynth -h
fluidsynth -o help
fluidsynth -a wasapi -o midi.driver=winmidi -o midi.winmidi.device="0:loopMIDI Port" C:\tools\fluidsynth\soundfonts\soundfont.sf2
```

The project soundfont is currently expected at `sf/soundfont.sf2`.
