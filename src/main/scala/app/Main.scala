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

// change queue to run each event on separated fiber
// scala cli migration
// complete readme
// to stop app send to all queues None

object Main extends IOApp.Simple {

  def program[F[_]: Async](tracks: List[Track])= for {
    env     <- ask
    streams <- tracks.traverse(_.eventStream[F])
  } yield ReactiveSynth.resource[F](streams, env.soundFontPath) 

  def run: IO[Unit] = {
    
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
      TimeGen(LazyList(8, 8, 4).repeatN(repeatCount)),
      DurationGen(LazyList(2, 3, 4).repeatN(repeatCount)),
      NoteGen(LazyList(60, 64, 67).repeatN(repeatCount))
    )    
    
    val env = Env(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), console = consoleImpl)
    program[IO](List(track4)).value.run(env).flatMap {
      case Left(e) => IO.println(s"Error: $e")
      case Right(synth) => synth.use { case (_, all) => all.compile.drain }
    }
  }
}
