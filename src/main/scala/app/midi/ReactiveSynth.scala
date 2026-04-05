package app.midi

import cats.data.*
import cats.effect._
import cats.syntax.all._
import fs2.Stream
import fs2.concurrent.{Channel => Fs2Channel}
import javax.sound.midi._
import app.*
import cats.MonadThrow
import java.nio.file.Paths
import scala.concurrent.duration.*
import org.typelevel.log4cats.Logger

object ReactiveSynth {

  private def sendCleanupMessages[F[_]: Async](receiver: Receiver): F[Unit] = {
    val allChannels = 0 until 16
    val controls    = List(120, 121, 123) // all sound off, reset controllers, all notes off
    allChannels.toList.traverse_ { channel =>
      controls.traverse_ { control =>
        Async[F].delay {
          val msg = new ShortMessage()
          msg.setMessage(ShortMessage.CONTROL_CHANGE, channel, control, 0)
          receiver.send(msg, -1)
        }
      }
    }
  }

  def getMidiDeviceInfo(infos: Array[MidiDevice.Info]): String =
    val info = new StringBuilder
    if infos.isEmpty then info.append("No MIDI devices found.")
    else
      info.append(s"Found ${infos.length} MIDI device(s):\n")
      info.append("-" * 80)

      infos.zipWithIndex.foreach { (di, index) =>
        val device          = MidiSystem.getMidiDevice(di)
        val maxReceivers    = device.getMaxReceivers // -1 = unlimited
        val maxTransmitters = device.getMaxTransmitters

        val deviceType =
          if maxReceivers != 0 && maxTransmitters != 0 then "IN/OUT"
          else if maxReceivers != 0 then "OUT (can receive messages → send to this device)"
          else if maxTransmitters != 0 then "IN  (can transmit messages → read from this device)"
          else "UNKNOWN"

        info.append(s"\n[$index] ${di.getName}\n")
        info.append(s"     Vendor:       ${di.getVendor}\n")
        info.append(s"     Description:  ${di.getDescription}\n")
        info.append(s"     Version:      ${di.getVersion}\n")
        info.append(s"     Type:         $deviceType\n")
        info.append(s"     MaxReceivers: $maxReceivers (-1 = unlimited)\n")
        info.append(s"     MaxTransmit:  $maxTransmitters (-1 = unlimited)\n")
        info.append(s"     Class:        ${device.getClass.getName}\n")
        info.append(s"     Is Open:      ${device.isOpen}\n")
        info.append("-" * 80)
      }
    info.toString()

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
    } yield synth

  def loadDevice[F[_]: Async](portName: String): F[MidiDevice] =
    Async[F].delay {
      val infos = MidiSystem.getMidiDeviceInfo
      infos
        .map(MidiSystem.getMidiDevice)
        .find(dev => dev.getDeviceInfo.getName.contains(portName) && dev.getMaxReceivers != 0)
        .getOrElse(
          throw new Exception(s"MIDI device with port name '$portName' not found\n\n${getMidiDeviceInfo(infos)}")
        )
    }

  def resource[F[_]: Async: Concurrent: Logger](
      midiStreams: List[Stream[F, Stream[F, ShortMessage]]],
      portName: String
  ): Resource[F, (List[Fs2Channel[F, Stream[F, ShortMessage]]], Stream[F, Unit])] =
    for {
      device <- Resource.make(loadDevice(portName))(d => Async[F].delay(d.close()))
      _ <- Resource.eval(
        Logger[F].info(s"Opening MIDI device: ${device.getDeviceInfo.getName}") *> Async[F].delay(device.open())
      )
      receiver <- Resource.make(Async[F].delay(device.getReceiver))(r =>
        sendCleanupMessages(r).attempt.void *> Async[F].delay(r.close())
      )
      channels <- Resource.eval(midiStreams.traverse(_ => Fs2Channel.unbounded[F, Stream[F, ShortMessage]]))
    } yield {
      // Each stream sends nested MIDI streams into its own channel and closes it when done.
      val inputStreams = midiStreams.zip(channels).map { case (stream, channel) =>
        stream.through(channel.sendAll)
      }
      // Channel streams consume nested streams and send messages to the synthesizer.
      val channelStreams = channels.map { channel =>
        channel.stream.evalMap { stream =>
          Spawn[F]
            .start(
              stream
                .evalMap(msg =>
                  Logger[F].debug(Message.fromShortMessage(msg)) *> Async[F]
                    .delay(receiver.send(msg, -1))
                )
                .compile
                .drain
            )
            .void
        }
      }
      val all = Stream(inputStreams: _*).parJoinUnbounded.merge(Stream(channelStreams: _*).parJoinUnbounded)
      (channels, all)
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
