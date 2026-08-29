package app.playback

import app.domain.*
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.syntax.all.*

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** Parses a single track file from disk into a domain `Track`.
  *
  * The format is intentionally simple and readable: each non-empty line is a `key=value` pair such as `channel=0`,
  * `time=1,2`, `duration=1`, `note=60`, `velocity=100`.
  */
object TrackFileCompiler {

  def compile(track: java.nio.file.Path): ValidatedNec[String, Track] = {
    val lines  = Files.readAllLines(track).asScala.toVector
    val values = lines.iterator.map(_.trim).filter(_.nonEmpty).map(parseLine).toMap

    val channel: Option[ValidatedNec[String, Channel]]      = values.get("channel").map(parseChannel)
    val time: Option[ValidatedNec[String, Seq[Double]]]     = values.get("time").map(parseDoubleSeq)
    val duration: Option[ValidatedNec[String, Seq[Double]]] = values.get("duration").map(parseDoubleSeq)
    val note: Option[ValidatedNec[String, Seq[Int]]]        = values.get("note").map(parseIntSeq)
    val velocity: Option[ValidatedNec[String, Seq[Int]]]    = values.get("velocity").map(parseIntSeq)

    (channel, time, duration, note, velocity) match {
      case (Some(Valid(ch)), Some(Valid(times)), Some(Valid(durs)), Some(Valid(notes)), Some(Valid(vels))) =>
        Track(
          channel = ch.validNec,
          timeGen = Generator.TimeGen(times),
          durGen = Generator.DurationGen(durs),
          noteGen = Generator.NoteGen(notes),
          velGen = Some(Generator.VelocityGen(vels))
        ).validNec[String]
      case _ =>
        "Invalid track file format".invalidNec[Track]
    }
  }

  private def parseLine(line: String): (String, String) = {
    val idx = line.indexOf('=')
    if (idx <= 0) then ("", line) else line.substring(0, idx).trim -> line.substring(idx + 1).trim
  }

  private def parseChannel(value: String): ValidatedNec[String, Channel] =
    value.toIntOption match {
      case Some(v) =>
        Channel.from(v) match {
          case Valid(ch)     => ch.validNec[String]
          case Invalid(errs) => s"Invalid channel: ${errs.toString}".invalidNec[Channel]
        }
      case None => s"Invalid channel value: $value".invalidNec[Channel]
    }

  private def parseDoubleSeq(value: String): ValidatedNec[String, Seq[Double]] =
    val parts = value.split(',').map(_.trim).toVector
    if (parts.forall(_.nonEmpty) && parts.forall(_.toDoubleOption.isDefined)) then
      parts.map(_.toDouble).validNec[String]
    else s"Invalid numeric sequence: $value".invalidNec[Seq[Double]]

  private def parseIntSeq(value: String): ValidatedNec[String, Seq[Int]] =
    val parts = value.split(',').map(_.trim).toVector
    if (parts.forall(_.nonEmpty) && parts.forall(_.toIntOption.isDefined)) then parts.map(_.toInt).validNec[String]
    else s"Invalid integer sequence: $value".invalidNec[Seq[Int]]
}
