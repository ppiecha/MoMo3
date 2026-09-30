package app.syntax

object Extensions {

  extension [A](seq: Seq[A]) {
    def repeat(count: Int): Seq[A] = if count <= 0 then Seq.empty else seq ++ seq.repeat(count - 1)
  }

//  extension (tracks: Tracks) {
//    def ++(other: Tracks): Tracks = ???

}
