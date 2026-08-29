package app.playback

/** Repeat strategy for a playback session after the current plan is completed.
  *
  * The policy follows the same semantics as the prompt: a fixed count repeats the playback N additional times, while
  * `Forever` continues indefinitely until the caller explicitly stops the controller.
  */
sealed trait PlaybackRepeatPolicy {
  def remaining: Int

  def shouldRepeat: Boolean = remaining != 0

  def next: PlaybackRepeatPolicy
}

object PlaybackRepeatPolicy {
  case object None extends PlaybackRepeatPolicy {
    override val remaining: Int             = 0
    override def next: PlaybackRepeatPolicy = this
  }

  final case class Fixed(count: Int) extends PlaybackRepeatPolicy {
    override val remaining: Int = math.max(0, count)

    override def next: PlaybackRepeatPolicy =
      if (count <= 1) None else Fixed(count - 1)
  }

  case object Forever extends PlaybackRepeatPolicy {
    override val remaining: Int             = Int.MaxValue
    override def next: PlaybackRepeatPolicy = this
  }

  def none: PlaybackRepeatPolicy = None

  def fixed(repeats: Int): PlaybackRepeatPolicy =
    if (repeats <= 0) None else Fixed(repeats)

  def forever: PlaybackRepeatPolicy = Forever
}
