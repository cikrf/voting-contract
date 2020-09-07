package com.wavesplatform.voting.contract

import java.math.BigInteger
import java.time.Instant

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.voting.contract.util.InstantUtil.InstantJsonFormat
import enumeratum.{Enum, EnumEntry, EnumFormats, PlayJsonEnum}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.Writes._
import play.api.libs.json._

import scala.collection.immutable
import scala.util.{Failure, Success, Try}

sealed trait PollType extends EnumEntry
object PollType extends Enum[PollType] with PlayJsonEnum[PollType] {

  override implicit val jsonFormat: Format[PollType] = EnumFormats.formats(this, insensitive = true)

  val values: immutable.IndexedSeq[PollType] = findValues

  case object Blind extends PollType
}

sealed trait VotingStatus extends EnumEntry
object VotingStatus extends Enum[VotingStatus] with PlayJsonEnum[VotingStatus] {

  override implicit val jsonFormat: Format[VotingStatus] = EnumFormats.formats(this, insensitive = true)

  val values: immutable.IndexedSeq[VotingStatus] = findValues

  case object Active    extends VotingStatus
  case object Halted    extends VotingStatus
  case object Completed extends VotingStatus
}

case class VotingBase(
  pollId: String,
  pollType: PollType,
  bulletinHash: String,
  dimension: Vector[Int],
  blindSigModulo: BigInteger,
  blindSigExponent: BigInteger,
  k: Long,
  dkgRound: Long,
  dateStart: Instant,
  dateEnd: Option[Instant],
  status: VotingStatus) {
  def toDataEntry: DataEntry = {
    DataEntry("VOTING_BASE", toJsonStr(this))
  }
}

object VotingBase extends InstantJsonFormat {

  private implicit val BigIntegerReads: Reads[BigInteger] = Reads {
    case JsString(value) =>
      Try(new BigInteger(value, 16)) match {
        case Success(bigInteger) => JsSuccess(bigInteger)
        case Failure(_)          => JsError(s"Invalid BigInteger number value '$value'")
      }
    case _ => JsError(s"Invalid BigInteger number value")
  }

  private implicit val BigIntegerWrites: Writes[BigInteger] = bigInteger => JsString(bigInteger.toString(16))

  implicit val BigIntegerFormat: Format[BigInteger] = Format(BigIntegerReads, BigIntegerWrites)

  implicit val votingBaseFormat: Format[VotingBase] = {
    ((JsPath \ "pollId").format[String] and
      (JsPath \ "pollType").format[PollType] and
      (JsPath \ "bulletinHash").format[String] and
      (JsPath \ "dimension").format[Vector[Int]] and
      (JsPath \ "blindSigModulo").format[BigInteger](BigIntegerFormat) and
      (JsPath \ "blindSigExponent").format[BigInteger](BigIntegerFormat) and
      (JsPath \ "k").format[Long] and
      (JsPath \ "dkgRound").format[Long] and
      (JsPath \ "dateStart").format[Instant] and
      (JsPath \ "dateEnd").formatNullable[Instant] and
      (JsPath \ "status").format[VotingStatus])(VotingBase.apply, unlift(VotingBase.unapply))
  }
}
