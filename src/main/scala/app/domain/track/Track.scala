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
  isActive: Boolean
) {

  def withSource(newSource: Option[String]): Track = this.copy(source = newSource)

  def muted: Track = {
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
    notes: Option[Generator[Note]],
    duration: Option[DurationGen] = None,
    velocity: Option[Generator[Velocity]] = None,
    isActive: Boolean = true,
    source: Option[String] = None
  )(using channel: Channel, instrument: Option[Instrument]): Track = {
    Track(
      timing,
      notes,
      duration,
      velocity,
      instrument,
      channel,
      source match {
        case Some(source) => Some(source)
        case None         => callerSource
      },
      isActive
    )
  }

  def rest(value: Double)(using channel: Channel): Track = {
    given Option[Instrument] = None
    track(
      timing = time(value),
      notes = None,
      duration = None,
      velocity = None
    )
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

  def note(head: NoteArg, tail: NoteArg*): Option[NoteGen] = {
    Some(Generator.note(head, tail*))
  }

  def velocity(head: Int, tail: Int*): Option[VelocityGen] = {
    Some(Generator.velocity(head, tail*))
  }
}
