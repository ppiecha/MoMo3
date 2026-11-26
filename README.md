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

Notes are written as pitch constants `c0` through `c8` (sharps use `s`, e.g. `cs2`; `c`, `cs`, � alias octave 4) or as MIDI numbers (0-127). `r` is a rest inside a pattern. Chords are tuples, e.g. `(c3, g4)`. `c1 * 15` repeats a note 15 times and is itself a note generator, so patterns combine with `++`. Percussion aliases: `bd`, `sn`, `clap`, `hhc`, `hhp`, `hho`.

A single `import app.dsl.{*, given}` provides the whole DSL:

| Helper | Meaning |
|--------|---------|
| `TrackSet(channel, instrument(program))` | channel and instrument for all tracks of an object (`instrument(bank, program)` selects a bank) |
| `steps(16, notes = �)` | track whose timing is derived from the notes: every note lasts `16` |
| `track(timing = �, notes = �)` | track with explicit timing |
| `note`, `velocity`, `duration`, `time` | generators; pass them to `track` / `steps` directly |
| `pause(n)` | silent track of length `n` (not to be confused with the in-pattern rest `r`) |
| `muted = true` | keeps the track defined but excludes it from playback |
| `track.silenced` | keeps the track but zeroes its velocity |
| `Section(a, b)` | tracks played in parallel |
When tracks are concatenated, an omitted duration uses that track's timing values,
an omitted note is a rest, and an omitted velocity uses the default velocity.
A velocity sequence shorter than the track's timing uses its first value for
the remaining steps. These defaults are applied separately to each track before
concatenation, so adding a rest or repeating a pattern preserves their alignment.
Muted segments remain silent when concatenated with active tracks; the active
segments still play. Pitch repetition with `*` requires a count of at least one.

## Example

### `Bass0.scala` � a melodic track

Each track file defines an `object` with a `TrackSet` (MIDI channel and instrument), then builds `Track`s from notes and velocity.

```scala
import app.dsl.{*, given}

object Bass0 {
  given TrackSet = TrackSet(Channel0, instrument(Bass.ElectricBassFinger))

  val bass: Track = steps(
    16,
    notes = (c1 * 7 ++ gs0 * 4 ++ f0 * 4 ++ as0 * 1).transpose(12),
    velocity = velocity(70)
  )
}
```

### `Drums9.scala` � a percussion track

Percussion uses channel 9. Tuples play several drums at the same step.

```scala
import app.dsl.{*, given}

object Drums9 {
  given TrackSet = TrackSet(Channel9, instrument(PercussionKit.ElectronicKit))

  val drums: Track = steps(
    8,
    notes = note((hhc, bd), hhp, (hhc, clap), hhp),
    velocity = velocity(60)
  ).repeat(8)
}
```

### `Music.scala` � the entrypoint

`Music` must be named like its file, extend `MusicFile` and define `play`, the `Section` to be played. A `Section` plays its tracks in parallel.

```scala
import app.dsl.*
import app.syntax.MusicFile

object Music extends MusicFile {

  override def bpm = Some(70)

  val rhythm = Section(
    Drums9.drums,
    Bass0.bass
  )

  def play: Section = rhythm
}
```
