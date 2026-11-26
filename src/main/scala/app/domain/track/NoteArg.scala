package app.domain

/** A single pitch (`60` or `c4`) or a chord given as a tuple, e.g. `(c3, g3)`. */
type NoteArg = Int | Pitch | Tuple
