package app.domain

import app.domain.Generator
import app.domain.Generator.NoteGen
import app.domain.Generator.VelocityGen
import app.syntax.Extensions.repeat
import cats.data.Ior
import cats.data.IorNec
import cats.data.NonEmptyChain as NEC
import cats.data.NonEmptyList

//@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
case class Track(
  timing: TimeGen,
  notes: Option[Generator[Note]],
  duration: Option[DurationGen],
  velocity: Option[Generator[Velocity]],
  instrument: Option[Instrument],
  channel: Channel,
  source: Option[String],
  muted: Boolean
) {

  def withSource(newSource: Option[String]): Track = this.copy(source = newSource)

  def silenced: Track = {
    this.copy(velocity = Some(Generator.velocity(Velocity.Zero).repeat(timing.length)))
  }

  def compareDuration(duration: Double): IorNec[DomainError, Track] = {
    if duration < 0 then Ior.Left(NEC.one(DomainError.NegativeDuration(duration)))
    else if timing.duration != duration then
      Ior.Left(NEC.one(DomainError.DurationMismatch(timing.duration, duration, source)))
    else Ior.Right(this)
  }

}

object Track {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def track(
    timing: TimeGen,
    notes: Generator[Note] | Option[Generator[Note]],
    duration: DurationGen | Option[DurationGen] = None,
    velocity: Generator[Velocity] | Option[Generator[Velocity]] = None,
    muted: Boolean = false,
    source: Option[String] = None
  )(using channel: Channel, instrument: Option[Instrument]): Track = {
    Track(
      timing,
      opt(notes),
      opt(duration),
      opt(velocity),
      instrument,
      channel,
      (if source.isDefined then source else callerSource),
      muted
    )
  }

  /** Track whose timing is derived from the notes: every note lasts `step`. */
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def steps(
    step: Double,
    notes: Generator[Note],
    duration: DurationGen | Option[DurationGen] = None,
    velocity: Generator[Velocity] | Option[Generator[Velocity]] = None,
    muted: Boolean = false,
    source: Option[String] = None
  )(using channel: Channel, instrument: Option[Instrument]): Track =
    track(
      TimeGen(NonEmptyList.fromListUnsafe(List.fill(notes.length)(step))),
      notes,
      duration,
      velocity,
      muted,
      (if source.isDefined then source else callerSource)
    )

  def pause(value: Double)(using channel: Channel): Track = {
    given Option[Instrument] = None
    track(
      timing = time(value),
      notes = note(0),
      duration = Track.duration(value),
      velocity = velocity(0)
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  private def opt[A](value: A | Option[A]): Option[A] = value match {
    case option: Option[?] => option.asInstanceOf[Option[A]]
    case other             => Some(other.asInstanceOf[A])
  }

  private def callerSource: Option[String] =
    new Throwable().getStackTrace.collectFirst {
      case frame if Option(frame.getFileName).exists(_.endsWith(".scala")) && frame.getFileName != "Track.scala" =>
        frame.getFileName
    }

  def time(head: Double, tail: Double*): TimeGen = {
    TimeGen(NonEmptyList(head, tail.toList))
  }

  def duration(head: Double, tail: Double*): DurationGen = {
    DurationGen(NonEmptyList(head, tail.toList))
  }

  def note(head: NoteArg, tail: NoteArg*): NoteGen = {
    Generator.note(head, tail*)
  }

  def velocity(head: Int, tail: Int*): VelocityGen = {
    Generator.velocity(head, tail*)
  }
}
