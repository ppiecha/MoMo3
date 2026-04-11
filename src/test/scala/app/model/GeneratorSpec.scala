package app.model

import munit.CatsEffectSuite
import cats.effect.IO
import app.midi.*
import app.*
import app.model.Generator.doubleToFiniteDuration
import scala.concurrent.duration.*

class GeneratorSpec extends CatsEffectSuite {

  test("doubleToFiniteDuration should convert double note duration to finite duration in millis based on BPM") {
    val testEnv = Environment(ppq = Ppq.unsafe(960), bpm = Bpm.unsafe(60), input = stdInput)
    val result = doubleToFiniteDuration[IO](8).value.run(testEnv).map {
      case Right(validated)  => validated
      case Left(domainError) => throw new RuntimeException(domainError.toString)
    }
    result.map { validated =>
      assert(validated.isValid)
      assertEquals(validated.getOrElse(0.millis), 500.millis)
    }
  }

}
