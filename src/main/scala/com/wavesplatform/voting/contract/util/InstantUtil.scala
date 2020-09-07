package com.wavesplatform.voting.contract.util

import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

import play.api.libs.json.{Format, Reads, Writes}

object InstantUtil {
  private val dateFormatPattern = "dd-MM-yyyy HH:mm:ss"
  private val dateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern(dateFormatPattern).withZone(ZoneOffset.UTC)

  def parse(str: String): Instant = {
    Instant.from(dateTimeFormatter.parse(str))
  }

  trait InstantJsonFormat {
    implicit val dateFormat: Format[Instant] = Format[Instant](
      Reads.instantReads[DateTimeFormatter](dateTimeFormatter),
      Writes.temporalWrites[Instant, DateTimeFormatter](dateTimeFormatter)
    )
  }
}
