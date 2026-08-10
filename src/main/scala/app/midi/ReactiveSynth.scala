package app.midi

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream

import javax.sound.midi.{MidiDevice, MidiSystem, Receiver, ShortMessage}
import app.config.{Environment, MidiOutputConfig}
import app.domain.{DomainError, ValidationError}
import cats.data.EitherT

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

  def loadDevice[F[_]: Async](portName: String): F[Either[DomainError, MidiDevice]] =
    Async[F]
      .delay {
        val infos = MidiSystem.getMidiDeviceInfo
        val maybeDevice =
          infos
            .map(MidiSystem.getMidiDevice)
            .find(dev => dev.getDeviceInfo.getName.contains(portName) && dev.getMaxReceivers != 0)

        maybeDevice.toRight(DomainError.ValidationFailed(ValidationError.InvalidPort(portName)))
      }

  def outputResource[F[_]: Async](
    cfg: MidiOutputConfig
  ): Resource[[A] =>> EitherT[F, DomainError, A], List[ShortMessage] => EitherT[F, DomainError, Unit]] =
    for {
      device <- Resource.eval(EitherT(loadDevice[F](cfg.loopMidiPortName)))
      _      <- Resource.eval(EitherT.liftF(Async[F].delay(device.open())))
      receiver <- Resource.make[[A] =>> EitherT[F, DomainError, A], Receiver](
        EitherT.liftF(Async[F].delay(device.getReceiver))
      ) { r =>
        EitherT.liftF(sendCleanupMessages(r) *> Async[F].delay(r.close()))
      }
    } yield { (messages: List[ShortMessage]) =>
      EitherT.liftF(Async[F].delay(messages.foreach(msg => receiver.send(msg, -1))))
    }

}
