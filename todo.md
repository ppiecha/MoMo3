# TODO

## Cel

Uprościć pipeline odtwarzania przez przejście z modelu opartego o zagnieżdżone strumienie na model oparty o jawne zdarzenia czasowe `AbsoluteMidiEvent`.

Docelowy przepływ:

`Track -> AbsoluteMidiEvent -> merge tracks -> sort by time -> scheduler -> MIDI output`

## P1 - Zmiany rdzeniowe

- Dodać typ `AbsoluteMidiEvent(at: Tick, command: MidiCommand)` jako centralną reprezentację zdarzenia odtwarzania.
- Ustalić `Tick` jako główne źródło prawdy o czasie w domenie i kompilacji.
- Przenieść przeliczanie `Tick -> FiniteDuration` do schedulera, zamiast trzymać czas wykonawczy w modelu domenowym.
- Przepisać `TrackCompiler`, żeby produkował bezpośrednio `AbsoluteMidiEvent` zamiast `Event`.
- Dla nuty generować dwa eventy absolutne: `NoteOn(start)` i `NoteOff(start + duration)`.
- Zmienić `CompiledTrack`, żeby przechowywał `LazyList[ErrorOr[AbsoluteMidiEvent]]`.
- Usunąć specjalne traktowanie `NoteOff` w runtime i skasować `MidiCommand.splitNoteOnOff`.
- Uprościć `PlaybackService` do jednego scalonego planu odtwarzania zamiast `Stream[F, Stream[F, ShortMessage]]`.
- Dodać jeden scheduler odpowiedzialny za:
  - sortowanie eventów po czasie
  - wyliczanie delt między kolejnymi eventami
  - `sleep`
  - wysyłkę komunikatów

## P2 - Uproszczenie modelu i warstw

- Usunąć rozdwojenie `Message` / `GeneratedMessage` i zostawić jeden model komunikatu.
- Ograniczyć albo usunąć `Event` z pipeline playbacku.
- Usunąć `EventConverter.eventToStreamOfMidiMessages`, bo playback nie powinien już tworzyć zagnieżdżonych streamów.
- Przenieść prostą konwersję `AbsoluteMidiEvent -> ShortMessage` oraz `AbsoluteMidiEvent -> MidiEvent` do jednego encodera.
- Uprościć `ReactiveSynth` do roli adaptera infrastrukturalnego:
  - open / close device
  - get receiver
  - send message
  - cleanup
- Usunąć z `ReactiveSynth` kanały, `observe`, `Spawn.start` i topologię opartą o wewnętrzne streamy.
- Oczyścić zależności między pakietami, tak aby `domain` nie importował infrastruktury MIDI.

## P3 - Stabilizacja i rozwój

- Ustalić stabilną kolejność eventów o tym samym czasie.
- Obsłużyć merge wielu ścieżek przez dane, nie przez wiele niezależnych mechanizmów runtime.
- Dodać testy dla:
  - pojedynczej nuty
  - akordu z wieloma `NoteOn` w tym samym czasie
  - różnych czasów `NoteOff`
  - merge kilku ścieżek
  - sortowania eventów
  - schedulera
- Uprościć `Main`, żeby tylko składał środowisko, kompilował tracki i uruchamiał playback.
- Posprzątać zbędne typy i pliki po migracji.

## Kolejność refaktoru

1. Dodać `AbsoluteMidiEvent`.
2. Przepisać `TrackCompiler` na produkcję eventów absolutnych.
3. Zmienić `CompiledTrack`.
4. Usunąć `splitNoteOnOff`.
5. Zastąpić `EventConverter` prostym encoderem.
6. Przepisać `PlaybackService` na jeden plan odtwarzania.
7. Uprościć `ReactiveSynth`.
8. Posprzątać `Event`, `GeneratedMessage` i `Message`.
9. Dodać testy.

## Docelowy minimalny zestaw elementów

- `Track`
- `Generator`
- `MidiCommand`
- `AbsoluteMidiEvent`
- `TrackCompiler`
- `PlaybackService`
- `ReactiveSynth`
- `JavaxMidiEncoder`
- `Environment`