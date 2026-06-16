# Functional Programming And Architecture Review

This file lists the most important corrections to make in the project, ordered from the most urgent to the least urgent. Each point explains what should be changed and why it matters, so the work can be done one item at a time.

## 1. Remove infrastructure and config dependencies from the domain layer

The domain package still imports application configuration and MIDI adapter concerns. Examples include `Generator` depending on `Environment`, `Tick` converting itself to milliseconds using `Environment`, and several domain files importing `app.config` or `app.midi` even when they do not need those modules directly.

What should be done:
- Make the domain depend only on domain types and pure abstractions.
- Move protocol encoding and playback-time conversion out of the domain.
- Replace `Tick.toMillis(env)` with a pure service or function in application/playback code, for example a `TimingContext(ppq, bpm)` or `Tempo` conversion module.
- Make `Generator.parse` take only the minimal domain values it needs, such as `Ppq`, instead of the whole `Environment`.

Why this is urgent:
- This is the main architectural leak in the project.
- It makes the domain harder to test, harder to reuse, and easier to accidentally couple to runtime details.

## 2. Stop using unsafe constructors in production paths

The code uses `unsafe` constructors for core domain values in places that are part of normal runtime behavior, not only in tests or fixtures. A concrete example is the hard-coded note velocity in `TrackCompiler`, but the same pattern appears in demo and setup code as well.

What should be done:
- Restrict `unsafe` constructors to tests, fixtures, or tightly controlled bootstrap code.
- In production code, construct domain values through validated smart constructors.
- Where invariants are guaranteed by construction, encapsulate the guarantee in a small dedicated factory instead of sprinkling `unsafe` calls across the codebase.

Why this is urgent:
- `unsafe` bypasses the main benefit of your value objects and weakens the correctness guarantees that FP-style domain modeling is supposed to provide.

## 3. Fix the MIDI encoding boundary so domain intent is translated correctly

The dedicated `MidiCommand` domain model is a good direction, but the adapter boundary is still incomplete. In particular, `ProgramChange` carries both `bank` and `program`, while the Javax encoder currently emits only a single `PROGRAM_CHANGE` message and ignores the bank selection part.

What should be done:
- Treat `app.midi.JavaxMidiEncoder` as the only place that knows `javax.sound.midi`.
- Encode `ProgramChange` into the correct sequence of MIDI messages, including bank select control changes before the program change.
- Add focused tests for each `MidiCommand` variant to verify protocol-level output.

Why this is urgent:
- This is both an architectural boundary problem and a behavioral bug.
- The current code can silently produce wrong output for valid domain commands.

## 4. Keep expected domain failures in typed results, not exceptions

The project models domain failures with `Either` and `Validated`, which is good, but then converts them to `DomainException` in application and MIDI code. That shifts expected failures into exception-based control flow too early.

What should be done:
- Keep expected failures in `Either`, `Validated`, `EitherT`, or `Kleisli` style flows until the outermost program boundary.
- Reserve exceptions for truly unexpected failures or Java interop boundaries.
- Refactor services like playback and MIDI device loading so the boundary that runs the program decides how to render or log domain failures.

Why this is important:
- It keeps the code more referentially transparent and easier to compose and test.
- It also makes failure channels explicit in function signatures.

## 5. Separate pure planning from effectful playback more strictly

`PlaybackService` contains a good seam with `toPlaybackPlan`, but the implementation still mixes validation, scheduling, sorting, delay calculation, and effect execution in one module.

What should be done:
- Create one pure module that turns compiled tracks into a deterministic playback plan.
- Create one effectful interpreter that executes that plan by sleeping and sending commands.
- Keep the effectful interpreter generic over `F[_]`, and keep all ordering and timing calculations pure.

Why this is important:
- This gives a cleaner FP architecture: algebra or plan on one side, interpreter on the other.
- It also makes scheduling logic easy to test without any effect runtime.

## 6. Remove eager collection building and quadratic list appends from playback code

The playback path materializes lazy structures with `toList.sequence`, sorts all events eagerly, and builds lists using `acc :+ item` inside a fold. This works for small examples but scales poorly and undermines the value of using lazy streams.

What should be done:
- Avoid `acc :+ item` in folds; build in reverse or use a more appropriate collection.
- Decide whether compilation is intentionally batch-oriented or stream-oriented, and model it consistently.
- If playback should handle larger tracks, move toward a streaming merge/scheduling model instead of collecting everything first.

Why this is important:
- It is a practical FP concern: preserving declarative composition without accidental performance traps.

## 7. Make the domain vocabulary more precise than raw MIDI aliases

`Note`, `Velocity`, `Bank`, `Program`, and `Control` are currently aliases of the same `MidiValue` opaque type. This improves validation reuse, but it does not prevent mixing semantically different values by mistake.

What should be done:
- Consider introducing separate opaque types or tiny wrappers for semantically distinct concepts that happen to share the same numeric range.
- At minimum, create dedicated constructors or modules for the concepts that should not be interchangeable.

Why this is important:
- Stronger domain semantics improve compiler help and reduce whole classes of accidental swaps.

## 8. Move syntax/extensions and utility behavior out of core domain files unless they are true domain concepts

The project defines generic `LazyList` extensions and conversions inside the domain area. These utilities are not domain concepts; they are general-purpose collection helpers.

What should be done:
- Move generic syntax helpers to a dedicated shared or syntax module.
- Keep the domain focused on business vocabulary and invariants.

Why this is important:
- It reduces conceptual noise and keeps domain modules easier to navigate.

## 9. Tighten the test strategy around architectural seams, not just happy-path outputs

Current tests cover some basic conversions and a happy path track compilation. They do not yet lock down critical boundaries such as encoding correctness, invalid input accumulation, playback ordering of simultaneous events, or failure propagation.

What should be done:
- Add unit tests for smart constructors and validation accumulation.
- Add adapter tests for Javax MIDI encoding.
- Add pure planning tests for playback ordering, equal timestamps, and delay calculation.
- Keep effectful integration tests separate from pure domain and application tests.

Why this is important:
- In FP-oriented code, strong small tests around pure boundaries are one of the main advantages. The current suite does not fully exploit that.

## 10. Split the current Environment into smaller capability-specific inputs

`Environment` currently mixes musical timing configuration, console input, MIDI port configuration, and soundfont path. This makes many functions depend on more context than they actually need.

What should be done:
- Replace the broad environment object with smaller records or parameters by concern.
- Examples: `TimingContext`, `MidiOutputConfig`, `ConsoleInput`, `SynthConfig`.
- Pass only the capability needed by a given function.

Why this is important:
- Smaller dependencies improve clarity, testability, and architectural boundaries.

## Suggested execution order

1. Clean the domain boundary and remove `Environment` or MIDI imports from domain.
2. Replace production `unsafe` usage with validated construction or dedicated factories.
3. Fix `JavaxMidiEncoder` behavior and add protocol-level tests.
4. Refactor error handling so expected failures stay typed.
5. Extract a pure playback planner from the current playback service.
6. Improve collection usage and streaming behavior.
7. Strengthen domain types and move generic syntax helpers out of domain.
8. Expand tests around the newly isolated pure seams.
9. Split `Environment` into smaller context objects.