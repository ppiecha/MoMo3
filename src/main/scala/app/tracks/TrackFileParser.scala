package app.tracks

import app.domain.*
import app.domain.Generator.*
import cats.syntax.all.*

object TrackFileParser {

  enum ParseError:
    case MissingKey(key: String)
    case InvalidValue(key: String, value: String)

  def parse(content: String): Either[ParseError, Track] =
    val props = content.linesIterator
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .flatMap { line =>
        val idx = line.indexOf('=')
        if idx < 0 then None
        else Some(line.take(idx).trim -> line.drop(idx + 1).trim)
      }
      .toMap

    for
      channelInt <- getInt(props, "channel")
      timeSeq    <- getDoubles(props, "time")
      durSeq     <- getDoubles(props, "duration")
      noteSeq    <- getInts(props, "notes")
      velGen <- props.get("velocity") match
        case None    => Right(None)
        case Some(_) => getInts(props, "velocity").map(s => Some(VelocityGen(s)))
    yield Track(
      channel = Channel.from(channelInt),
      timeGen = TimeGen(timeSeq),
      durGen = DurationGen(durSeq),
      noteGen = NoteGen(noteSeq),
      velGen = velGen
    )

  private def getInt(props: Map[String, String], key: String): Either[ParseError, Int] =
    props
      .get(key)
      .toRight(ParseError.MissingKey(key))
      .flatMap(v => v.toIntOption.toRight(ParseError.InvalidValue(key, v)))

  private def getDoubles(props: Map[String, String], key: String): Either[ParseError, Seq[Double]] =
    props
      .get(key)
      .toRight(ParseError.MissingKey(key))
      .flatMap(
        _.split(",").toList.traverse(d => d.trim.toDoubleOption.toRight(ParseError.InvalidValue(key, d.trim)))
      )

  private def getInts(props: Map[String, String], key: String): Either[ParseError, Seq[Int]] =
    props
      .get(key)
      .toRight(ParseError.MissingKey(key))
      .flatMap(
        _.split(",").toList.traverse(i => i.trim.toIntOption.toRight(ParseError.InvalidValue(key, i.trim)))
      )
}
