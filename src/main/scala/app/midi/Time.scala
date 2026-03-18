package app.midi

import scala.concurrent.duration.*

case class Time(duration: FiniteDuration, tick: Tick)
object Time {
  def zero: Time = Time(0.millis, Tick.unsafe(0))
}
