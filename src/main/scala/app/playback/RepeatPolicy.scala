package app.playback

/** Repeat strategy for a playback session after the current plan is completed.
  *
  * The policy follows the same semantics as the prompt: a fixed count repeats the playback N additional times, while
  * `Forever` continues indefinitely until the caller explicitly stops the controller.
  */
sealed trait RepeatPolicy {
  def remaining: Int
  def shouldRepeat: Boolean = remaining != 0
  def next: RepeatPolicy
}

object RepeatPolicy {
  case object None extends RepeatPolicy {
    override val remaining: Int     = 0
    override def next: RepeatPolicy = this
  }

  final case class Fixed(count: Int) extends RepeatPolicy {
    override val remaining: Int = math.max(0, count)

    override def next: RepeatPolicy =
      if (count <= 1) None else Fixed(count - 1)
  }

  case object Forever extends RepeatPolicy {
    override val remaining: Int     = Int.MaxValue
    override def next: RepeatPolicy = this
  }

  def none: RepeatPolicy = None

  def fixed(repeats: Int): RepeatPolicy =
    if (repeats <= 0) None else Fixed(repeats)

  def forever: RepeatPolicy = Forever
}
