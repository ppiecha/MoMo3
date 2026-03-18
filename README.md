Run loopMIDI

cd C:\tools\fluidsynth\bin

.\fluidsynth.exe -h
.\fluidsynth.exe -o help

//fluidsynth -a dsound -o midi.driver=winmidi -o midi.winmidi.device="0:loopMIDI Port" C:\tools\fluidsynth\soundfonts\soundfont.sf2
fluidsynth -a wasapi -o midi.driver=winmidi -o midi.winmidi.device="0:loopMIDI Port" C:\tools\fluidsynth\soundfonts\soundfont.sf2

  // --- szukanie portu loopMIDI ---
  def findPort(name: String): Option[MidiDevice] =
    val infos = MidiSystem.getMidiDeviceInfo()
    println(s"\nDostępne urządzenia MIDI (${infos.length}):")
    infos.zipWithIndex.foreach { (info, i) =>
      val dev = MidiSystem.getMidiDevice(info)
      val dir = if dev.getMaxReceivers != 0 then "OUT" else "IN"
      println(s"  [$i] $dir  ${info.getName}  (${dev.getClass.getSimpleName})")
    }
    println()

    infos
      .map(MidiSystem.getMidiDevice)
      .find { dev =>
        dev.getDeviceInfo.getName.contains(name) && dev.getMaxReceivers != 0
      }

findPort(PortName) match
      case Some(device) =>
        device.open()
        val receiver = device.getReceiver
        println(s"✅ Połączono z: ${device.getDeviceInfo.getName}\n")
finally
    receiver.close()
    device.close()



## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
