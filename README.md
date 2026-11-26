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

While running, the console accepts single-key commands:

| Key | Action                                 |
|-----|----------------------------------------|
| `p` | Pause / resume playback                |
| `s` | Start / stop playback                  |
| `r` | Restart playback                       |
| `n` | Reload `application.conf`              |
| `c` | Force a rescan of the tracks directory |
| `q` | Quit                                   |

## Configuration

Settings live in `src/main/resources/application.conf`:

```hocon
timing-context {
  bpm = 60            # default tempo, used when Music does not override bpm
  ppq = 960           # MIDI resolution (pulses per quarter note)
}

midi-output-config {
  loop-midi-port-name = "ScalaToFluid"   # MIDI output port to send events to
}

paths {
  tracks = "projects/stranger/"                    # directory watched for track files
  music-file = "projects/stranger/Music.scala"     # music entrypoint
  common-file = "projects/stranger/Common.scala"
  polling-interval = 100                           # directory polling interval (ms)
}

start-playback-on-startup = true
```

## Track files

Project directories live under `projects/` and are compiled together with the application. Every file in the configured `paths.tracks` directory is watched; saving a file recompiles it and updates playback.

The music entrypoint must define an `object` with the same name as its Scala file (for example, `Music` in `Music.scala`). It may use a Scala package; its package is determined from the compiled object, not the directory name. If the compiled sources contain objects with the same name in different packages, the entrypoint is ambiguous and must be renamed.

An entrypoint extending `MusicFile` can override `def bpm: Option[Int] = Some(120)`.
The BPM must be positive; if omitted, `timing-context.bpm` from the application configuration is used.
Saving a changed BPM rebuilds playback at the same musical position. Invalid BPM leaves the previous plan active.

Notes can be specified as MIDI numbers (0-127) or by pitch constants from `Note.c0` through `Note.c8`. Sharps use `s` (for example `Note.cs2`); names without an octave such as `Note.c` and `Note.cs` alias octave 4. `Note.r` is a rest. Use `import app.domain.Note.*` to write notes without the `Note.` prefix. Chords are written as tuples, e.g. `(c3, g4)`.

```scala
Track.note(Note.a0, Note.as0, Note.b0, Note.c1, Note.cs1, Note.c8)
```

When tracks are concatenated, an omitted duration uses that track's timing values,
an omitted note is a rest, and an omitted velocity uses the default velocity.
A velocity sequence shorter than the track's timing uses its first value for
the remaining steps. These defaults are applied separately to each track before
concatenation, so adding a rest or repeating a pattern preserves their alignment.

## Example

### `Bass0.scala` – a melodic track

Each track file defines an `object` with a MIDI channel and instrument as `given`s, then builds `Track`s from timing, notes and velocity.

```scala
import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Note.*
import app.domain.Track.*
import app.syntax.Extensions.*
import app.typeclasses.Transposable.given

import scala.language.implicitConversions

object Bass0 {
  given Channel            = Channel0
  given Option[Instrument] = instrument(Bank.Default, Bass.ElectricBassFinger)

  val bass: Track = track(
    timing = time(16).repeat(16),
    notes = note(c1).repeat(7) ++
      note(gs0).repeat(4) ++
      note(f0).repeat(4) ++
      note(as0),
    velocity = velocity(70)
  ).transpose(12)
}
```

### `Drums9.scala` – a percussion track

Percussion uses channel 9. Tuples play several drums at the same step.

```scala
import app.domain.*
import app.domain.Channel.*
import app.domain.Instrument.*
import app.domain.Track.*
import app.syntax.Extensions.*

import scala.language.implicitConversions

object Drums9 {
  given Channel = Channel9
  given Option[Instrument] = instrument(
    bank = Bank.Default,
    program = PercussionKit.ElectronicKit
  )

  val drums = track(
    timing = time(8).repeat(4),
    notes = note(
      (PercussionNote.HiHat.ClosedHiHat, PercussionNote.BassDrum.BassDrum1),
      PercussionNote.HiHat.PedalHiHat,
      (PercussionNote.HiHat.ClosedHiHat, PercussionNote.Snare.HandClap),
      PercussionNote.HiHat.PedalHiHat
    ),
    velocity = velocity(60)
  ).repeat(8)
}
```

### `Music.scala` – the entrypoint

`Music` must be named like its file, extend `MusicFile` and define `play`, the `Section` to be played. A `Section` plays its tracks in parallel.

```scala
import app.domain.*
import app.syntax.MusicFile

object Music extends MusicFile {

  override def bpm = Some(70)

  val rhythm = Section.of(
    Drums9.drums,
    Bass0.bass
  )

  def play: Section = rhythm
}
```
