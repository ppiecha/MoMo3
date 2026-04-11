import cats.effect._
import fs2.Stream
import javax.sound.midi._

import app.*
import app.midi.*
import app.model.Track

import cats.syntax.all.*

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.DurationInt

// modify main to print/play list of tracks stopping after longest track
// complete readme
// fix duration test

object Main extends IOApp.Simple {

  def program[F[_]: Async: Logger](tracks: List[Track]) = for {
    env     <- ask
    outputs <- tracks.traverse(_.output[F])
    //events  <- liftF(Async[F].delay(outputs.flatMap(_.midiEventList.toList)))
  } yield (ReactiveSynth.resource[F](outputs.map(_.midiStream), env), outputs)

  def run: IO[Unit] = {
    import Tracks.*
    Slf4jLogger.create[IO].flatMap { logger =>
      given Logger[IO] = logger

      val repeatCount = 1

      val env = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
      val print = track4.eventListToString[IO](10).value.run(env).flatMap {
        case Left(e) => logger.error(s"Error: $e")
        case Right(str) => logger.info(s"Event list:\n$str")
      }
      val play = program[IO](List(track4)).value.run(env).flatMap {
        case Left(e) => logger.error(s"Error: $e")
        case Right((synth, outputs)) =>
          outputs.toList
            .traverse_(o => o.midiEventList.take(10).toList.traverse_(e => logger.info(s"Track event: ${e.toString2}"))) *>
            synth.use { case (_, all) => all.compile.drain *> IO.sleep(2.second) }
      }
      print *> play
    }
  }
}
