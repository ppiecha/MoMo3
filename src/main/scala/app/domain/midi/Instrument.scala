package app.domain

case class Instrument(bank: Int, program: Int)

object Instrument {

  val Default: Instrument = Instrument(Bank.Melodic, Piano.AcousticGrandPiano)

  object Bank {
    val Melodic: Int    = 0
    val Percussion: Int = 128
  }

  object Piano {
    val AcousticGrandPiano: Int  = 0
    val BrightAcousticPiano: Int = 1
    val ElectricGrandPiano: Int  = 2
    val HonkyTonkPiano: Int      = 3
    val ElectricPiano1: Int      = 4
    val ElectricPiano2: Int      = 5
    val Harpsichord: Int         = 6
    val Clavinet: Int            = 7
  }

  object ChromaticPercussion {
    val Celesta: Int      = 8
    val Glockenspiel: Int = 9
    val MusicBox: Int     = 10
    val Vibraphone: Int   = 11
    val Marimba: Int      = 12
    val Xylophone: Int    = 13
    val TubularBells: Int = 14
    val Dulcimer: Int     = 15
  }

  object Organ {
    val DrawbarOrgan: Int    = 16
    val PercussiveOrgan: Int = 17
    val RockOrgan: Int       = 18
    val ChurchOrgan: Int     = 19
    val ReedOrgan: Int       = 20
    val Accordion: Int       = 21
    val Harmonica: Int       = 22
    val TangoAccordion: Int  = 23
  }

  object Guitar {
    val AcousticGuitarNylon: Int = 24
    val AcousticGuitarSteel: Int = 25
    val ElectricGuitarJazz: Int  = 26
    val ElectricGuitarClean: Int = 27
    val ElectricGuitarMuted: Int = 28
    val OverdrivenGuitar: Int    = 29
    val DistortionGuitar: Int    = 30
    val GuitarHarmonics: Int     = 31
  }

  object Bass {
    val AcousticBass: Int       = 32
    val ElectricBassFinger: Int = 33
    val ElectricBassPick: Int   = 34
    val FretlessBass: Int       = 35
    val SlapBass1: Int          = 36
    val SlapBass2: Int          = 37
    val SynthBass1: Int         = 38
    val SynthBass2: Int         = 39
  }

  object Strings {
    val Violin: Int           = 40
    val Viola: Int            = 41
    val Cello: Int            = 42
    val Contrabass: Int       = 43
    val TremoloStrings: Int   = 44
    val PizzicatoStrings: Int = 45
    val OrchestralHarp: Int   = 46
    val Timpani: Int          = 47
  }

  object Ensemble {
    val StringEnsemble1: Int = 48
    val StringEnsemble2: Int = 49
    val SynthStrings1: Int   = 50
    val SynthStrings2: Int   = 51
    val ChoirAahs: Int       = 52
    val VoiceOohs: Int       = 53
    val SynthVoice: Int      = 54
    val OrchestraHit: Int    = 55
  }

  object Brass {
    val Trumpet: Int      = 56
    val Trombone: Int     = 57
    val Tuba: Int         = 58
    val MutedTrumpet: Int = 59
    val FrenchHorn: Int   = 60
    val BrassSection: Int = 61
    val SynthBrass1: Int  = 62
    val SynthBrass2: Int  = 63
  }

  object Reed {
    val SopranoSax: Int  = 64
    val AltoSax: Int     = 65
    val TenorSax: Int    = 66
    val BaritoneSax: Int = 67
    val Oboe: Int        = 68
    val EnglishHorn: Int = 69
    val Bassoon: Int     = 70
    val Clarinet: Int    = 71
  }

  object Pipe {
    val Piccolo: Int     = 72
    val Flute: Int       = 73
    val Recorder: Int    = 74
    val PanFlute: Int    = 75
    val BlownBottle: Int = 76
    val Shakuhachi: Int  = 77
    val Whistle: Int     = 78
    val Ocarina: Int     = 79
  }

  object SynthLead {
    val Lead1Square: Int   = 80
    val Lead2Sawtooth: Int = 81
    val Lead3Calliope: Int = 82
    val Lead4Chiff: Int    = 83
    val Lead5Charang: Int  = 84
    val Lead6Voice: Int    = 85
    val Lead7Fifths: Int   = 86
    val Lead8BassLead: Int = 87
  }

  object SynthPad {
    val Pad1NewAge: Int    = 88
    val Pad2Warm: Int      = 89
    val Pad3Polysynth: Int = 90
    val Pad4Choir: Int     = 91
    val Pad5Bowed: Int     = 92
    val Pad6Metallic: Int  = 93
    val Pad7Halo: Int      = 94
    val Pad8Sweep: Int     = 95
  }

  object SynthEffects {
    val Fx1Rain: Int       = 96
    val Fx2Soundtrack: Int = 97
    val Fx3Crystal: Int    = 98
    val Fx4Atmosphere: Int = 99
    val Fx5Brightness: Int = 100
    val Fx6Goblins: Int    = 101
    val Fx7Echoes: Int     = 102
    val Fx8SciFi: Int      = 103
  }

  object Ethnic {
    val Sitar: Int    = 104
    val Banjo: Int    = 105
    val Shamisen: Int = 106
    val Koto: Int     = 107
    val Kalimba: Int  = 108
    val Bagpipe: Int  = 109
    val Fiddle: Int   = 110
    val Shanai: Int   = 111
  }

  object Percussive {
    val TinkleBell: Int    = 112
    val Agogo: Int         = 113
    val SteelDrums: Int    = 114
    val Woodblock: Int     = 115
    val TaikoDrum: Int     = 116
    val MelodicTom: Int    = 117
    val SynthDrum: Int     = 118
    val ReverseCymbal: Int = 119
  }

  object SoundEffects {
    val GuitarFretNoise: Int = 120
    val BreathNoise: Int     = 121
    val Seashore: Int        = 122
    val BirdTweet: Int       = 123
    val TelephoneRing: Int   = 124
    val Helicopter: Int      = 125
    val Applause: Int        = 126
    val Gunshot: Int         = 127
  }

  object PercussionKit {
    val StandardKit: Int   = 0
    val RoomKit: Int       = 8
    val PowerKit: Int      = 16
    val ElectronicKit: Int = 24
    val Tr808Kit: Int      = 25
    val JazzKit: Int       = 32
    val BrushKit: Int      = 40
    val OrchestraKit: Int  = 48
    val SfxKit: Int        = 56
  }

  object PercussionNote {
    object BassDrum {
      val AcousticBassDrum: Int = 35
      val BassDrum1: Int        = 36
    }

    object Snare {
      val SideStick: Int     = 37
      val AcousticSnare: Int = 38
      val HandClap: Int      = 39
      val ElectricSnare: Int = 40
    }

    object Toms {
      val LowFloorTom: Int  = 41
      val HighFloorTom: Int = 43
      val LowTom: Int       = 45
      val LowMidTom: Int    = 47
      val HiMidTom: Int     = 48
      val HighTom: Int      = 50
    }

    object HiHat {
      val ClosedHiHat: Int = 42
      val PedalHiHat: Int  = 44
      val OpenHiHat: Int   = 46
    }

    object Cymbals {
      val CrashCymbal1: Int  = 49
      val RideCymbal1: Int   = 51
      val ChineseCymbal: Int = 52
      val RideBell: Int      = 53
      val Tambourine: Int    = 54
      val SplashCymbal: Int  = 55
      val CrashCymbal2: Int  = 57
      val RideCymbal2: Int   = 59
    }

    object Latin {
      val HighBongo: Int     = 60
      val LowBongo: Int      = 61
      val MuteHighConga: Int = 62
      val OpenHighConga: Int = 63
      val LowConga: Int      = 64
      val HighTimbale: Int   = 65
      val LowTimbale: Int    = 66
      val HighAgogo: Int     = 67
      val LowAgogo: Int      = 68
      val Cabasa: Int        = 69
      val Maracas: Int       = 70
      val ShortWhistle: Int  = 71
      val LongWhistle: Int   = 72
      val ShortGuiro: Int    = 73
      val LongGuiro: Int     = 74
      val Claves: Int        = 75
      val HighWoodBlock: Int = 76
      val LowWoodBlock: Int  = 77
      val MuteCuica: Int     = 78
      val OpenCuica: Int     = 79
      val MuteTriangle: Int  = 80
      val OpenTriangle: Int  = 81
    }
  }
}
