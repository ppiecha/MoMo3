package app.typeclasses
import app.domain.Generator
import app.domain.Generator.NoteGen
import app.domain.Note
import app.domain.Track

trait Transposable[A] {
  def transpose(value: A, semitones: Int): A
}

object Transposable {

  given Transposable[NoteGen] with {
    def transpose(noteGen: NoteGen, semitones: Int): NoteGen = {
      NoteGen(noteGen.steps.map {
        case app.domain.Chord.Voiced(notes) =>
          app.domain.Chord.Voiced(notes.map(n => if n == 0 then n else n + semitones))
        case app.domain.Chord.Rest => app.domain.Chord.Rest
      })
    }
  }

  given [A](using transposable: Transposable[A]): Transposable[Option[A]] with {
    def transpose(optA: Option[A], semitones: Int): Option[A] = optA.map(v => transposable.transpose(v, semitones))
  }

  given (using transposable: Transposable[NoteGen]): Transposable[Generator[Note]] with {
    def transpose(generator: Generator[Note], semitones: Int): Generator[Note] =
      generator match {
        case noteGen: NoteGen => transposable.transpose(noteGen, semitones)
      }
  }

  given (using transposable: Transposable[Generator[Note]]): Transposable[Track] with {
    def transpose(track: Track, semitones: Int): Track =
      track.copy(notes = track.notes.map(n => transposable.transpose(n, semitones)))
  }

}
