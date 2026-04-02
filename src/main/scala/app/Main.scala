import cats.effect._
import fs2.Stream
import javax.sound.midi._
import app.midi.*
import app.*
import app.model.Track
import app.model.Generator.*
import cats.syntax.all.*
import app.model.given
import app.model.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

// complete readme

object Main extends IOApp.Simple {

  def program[F[_]: Async: Logger](tracks: List[Track]) = for {
    env     <- ask
    outputs <- tracks.traverse(_.output[F])
    //events  <- liftF(Async[F].delay(outputs.flatMap(_.midiEventList.toList)))
  } yield (ReactiveSynth.resource[F](outputs.map(_.midiStream), env.loopMidiPortName), outputs)

  def run: IO[Unit] = {
    Slf4jLogger.create[IO].flatMap { logger =>
      given Logger[IO] = logger

      val repeatCount = 1

      val track1 = Track(
        channel = Channel.unsafe(9),
        TimeGen(LazyList(8, 8, 4).repeatN(repeatCount)),
        DurationGen(LazyList(8, 8, 8).repeatN(repeatCount)),
        NoteGen(LazyList(36, 36, 0).repeatN(repeatCount))
      )

      val track2 = Track(
        channel = Channel.unsafe(9),
        TimeGen(LazyList(4, 4).repeatN(repeatCount)),
        DurationGen(LazyList(4, 4).repeatN(repeatCount)),
        NoteGen(LazyList(0, 39).repeatN(repeatCount))
      )

      val track3 = Track(
        channel = Channel.unsafe(9),
        TimeGen(LazyList(8, 8, 8, 8).repeatN(repeatCount)),
        DurationGen(LazyList(8, 8, 8, 8).repeatN(repeatCount)),
        NoteGen(LazyList(36, 36, 36, 36).repeatN(repeatCount))
      )

      val track4 = Track(
        channel = Channel.unsafe(0),
        TimeGen(LazyList(4, 4, 2).repeatN(repeatCount)),
        DurationGen(LazyList(1, 1.3, 2).repeatN(repeatCount)),
        NoteGen(LazyList(60, 64, 67).repeatN(repeatCount))
      )

      val env = Env(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
      program[IO](List(track4)).value.run(env).flatMap {
        case Left(e) => logger.error(s"Error: $e")
        case Right((synth, outputs)) =>
          outputs.toList
            .traverse_(o => o.midiEventList.take(10).toList.traverse_(e => logger.info(s"Track event: ${e.toString2}"))) *>
            synth.use { case (_, all) => all.compile.drain }
      }
    }
  }
}
