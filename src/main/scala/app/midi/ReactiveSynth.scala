package app.midi

import cats.data.*
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2.Stream
import javax.sound.midi._
import app.*
import cats.MonadThrow
import java.nio.file.Paths
import scala.concurrent.duration.*

object ReactiveSynth {

  def loadSynthesizer[F[_]: Async: MonadThrow](soundFontPath: String): F[Synthesizer] =
    for {
      synth     <- Async[F].delay(MidiSystem.getSynthesizer)
      sfFile    <- Async[F].blocking(Paths.get(soundFontPath).toFile)
      soundBank <- Async[F].delay(MidiSystem.getSoundbank(sfFile))
      supported <- Async[F].delay(synth.isSoundbankSupported(soundBank))
      _ <-
        if supported then Async[F].unit
        else MonadThrow[F].raiseError(new Exception(s"Soundbank not supported: $soundFontPath"))
      loaded <- Async[F].delay(synth.loadAllInstruments(soundBank))
      // todo
      // _ <-
      //   if loaded then Async[F].unit
      //   else MonadThrow[F].raiseError(new Exception(s"Failed to load soundbank: $soundFontPath"))
    } yield synth

  def loadDevice[F[_]: Async](portName: String): F[MidiDevice] =
    Async[F].delay {
      MidiSystem.getMidiDeviceInfo
        .map(MidiSystem.getMidiDevice)
        .find(dev => dev.getDeviceInfo.getName.contains(portName) && dev.getMaxReceivers != 0)
        .getOrElse(throw new Exception(s"MIDI device with port name '$portName' not found"))
    }

  def resource[F[_]: Async: Concurrent](
      midiStreams: List[Stream[F, Stream[F, MidiMessage]]],
      portName: String
  ): Resource[F, (List[Queue[F, Option[Stream[F, MidiMessage]]]], Stream[F, Unit])] =
    for {
      device   <- Resource.make(loadDevice(portName))(d => Async[F].delay(d.close()))
      _        <- Resource.eval(Async[F].delay(device.open()) *> Async[F].delay(println("Device ready...")))
      receiver <- Resource.make(Async[F].delay(device.getReceiver))(r => Async[F].delay(r.close()))
      queues   <- Resource.eval(midiStreams.traverse(_ => Queue.unbounded[F, Option[Stream[F, MidiMessage]]]))
    } yield {
      // Each stream puts messages into its own queue
      val inputStreams = midiStreams.zip(queues).map { case (stream, queue) =>
        stream.evalMap(s => queue.offer(Some(s)))
      }
      // Queue streams consume messages and send them to the synthesizer
      val queueStreams = queues.map { queue =>
        Stream
          .fromQueueNoneTerminated(queue)
          .evalMap( /* stream -> IO */ stream =>
            Spawn[F].start(stream.evalMap(msg => Async[F].delay(receiver.send(msg, -1))).compile.drain).void
          )
      }
      // Start all streams (can be merged or run in parallel)
      val all = Stream(inputStreams: _*).parJoinUnbounded.merge(Stream(queueStreams: _*).parJoinUnbounded)
      (queues, all)
    }

  def midiStreamFromFile[F[_]: Async](filePath: String): Stream[F, MidiMessage] =
    Stream
      .awakeEvery[F](1.second) // co sekundę
      .evalMap { _ =>
        Async[F].blocking {
          // wczytaj plik, sparsuj i zwróć listę wiadomości MIDI
          val messages: List[MidiMessage] = ??? // parseMidiFile(filePath)
          messages
        }
      }
      .flatMap(Stream.emits) // emituj każdą wiadomość osobno

}
