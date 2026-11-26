package app.dsl

import app.domain.Channel
import app.domain.Instrument

/** Channel and instrument shared by every track of a file: `given TrackSet = TrackSet(Channel0, instrument(20))`. */
final case class TrackSet(channel: Channel, instrument: Option[Instrument])

object TrackSet {
  given channelOf(using set: TrackSet): Channel               = set.channel
  given instrumentOf(using set: TrackSet): Option[Instrument] = set.instrument
}
