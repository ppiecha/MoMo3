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

1. Install fluid-synth from https://www.fluidsynth.org/wiki/Download/#distributions \
2. Install loopmidi from https://www.tobias-erichsen.de/software/loopmidi.html \
3. Create a virtual MIDI port in loopMIDI, e.g. "ScalaToFluid" \
4. Start FluidSynth with the following command, replacing the path to your soundfont file as

```powershell
fluidsynth -a wasapi -o midi.driver=winmidi -o midi.winmidi.device="0:ScalaToFluid" C:\tools\fluidsynth\soundfonts\soundfont.sf2
```
