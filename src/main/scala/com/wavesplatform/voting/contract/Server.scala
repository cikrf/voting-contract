package com.wavesplatform.voting.contract

import com.wavesplatform.protobuf.common.DataEntry
import enumeratum._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.immutable

sealed trait ServerType extends EnumEntry
object ServerType extends Enum[ServerType] with PlayJsonEnum[ServerType] {

  override implicit val jsonFormat: Format[ServerType] = EnumFormats.formats(this, insensitive = true)

  val values: immutable.IndexedSeq[ServerType] = findValues

  case object Decrypt extends ServerType
  case object Main    extends ServerType
}

sealed trait Status extends EnumEntry
object Status extends Enum[Status] with PlayJsonEnum[Status] {

  override implicit val jsonFormat: Format[Status] = EnumFormats.formats(this, insensitive = true)

  val values: immutable.IndexedSeq[Status] = findValues

  case object Active  extends Status
  case object Blocked extends Status
}

case class Point(x: BigInt, y: BigInt)
object Point {
  private implicit val bigIntWrite: Writes[BigInt] =
    bigInt => JsString(bigInt.toString())

  private implicit val bigIntRead: Reads[BigInt] = Reads {
    case JsString(value) => JsSuccess(scala.math.BigInt(value))
    case JsNumber(value) => JsSuccess(value.toBigInt())
    case _               => JsError(s"Invalid BigInt")
  }

  implicit val pointFormat: OFormat[Point] = Json.format[Point]
}

sealed abstract class ServerTrait {
  val pubKey: String
  val description: String
  val i: Int
  val `type`: ServerType
  val status: Status
  val dkgRound: Long
  val dkgScalar: String
  val dkgCommit: String
  val dkgShadows: String
  val dkgExponents: String
  val dkgComplaints: Vector[String]

  def block: ServerTrait
  def active: ServerTrait
  def pubKey(newValue: String): ServerTrait
  def i(newValue: Int): ServerTrait
  def withRound(round: Long): ServerTrait
  def withScalar(scalar: String): ServerTrait
  def withCommit(commit: String): ServerTrait
  def withShadows(shadows: String): ServerTrait
  def withExponents(exponents: String): ServerTrait
  def withComplaints(complaints: Vector[String]): ServerTrait

  def toDataEntry: DataEntry = {
    DataEntry(s"SERVER_$pubKey", toJsonStr(this))
  }
}

object ServerTrait {
  final case class DecryptServer(
    description: String,
    pubKey: String,
    status: Status,
    dkgRound: Long,
    dkgScalar: String,
    dkgCommit: String,
    dkgShadows: String,
    dkgComplaints: Vector[String],
    dkgExponents: String,
    i: Int)
    extends ServerTrait {
    override def block: ServerTrait                                      = this.copy(status = Status.Blocked)
    override def active: ServerTrait                                     = this.copy(status = Status.Active)
    override def pubKey(newValue: String): ServerTrait                   = this.copy(pubKey = newValue)
    override def i(newValue: Int): ServerTrait                           = this.copy(i = newValue)
    override def withRound(round: Long): ServerTrait                     = this.copy(dkgRound = round)
    override def withScalar(scalar: String): ServerTrait                 = this.copy(dkgScalar = scalar)
    override def withCommit(commit: String): ServerTrait                 = this.copy(dkgCommit = commit)
    override def withShadows(shadows: String): ServerTrait               = this.copy(dkgShadows = shadows)
    override def withExponents(exponents: String): ServerTrait           = this.copy(dkgExponents = exponents)
    override def withComplaints(complaints: Vector[String]): ServerTrait = this.copy(dkgComplaints = complaints)
    override val `type`: ServerType                                      = ServerType.Decrypt
  }

  final case class MainServer(
    description: String,
    pubKey: String,
    status: Status,
    dkgRound: Long,
    dkgScalar: String,
    dkgCommit: String,
    dkgShadows: String,
    dkgComplaints: Vector[String],
    dkgExponents: String,
    i: Int)
    extends ServerTrait {
    override def block: ServerTrait                                      = this.copy(status = Status.Blocked)
    override def active: ServerTrait                                     = this.copy(status = Status.Active)
    override def pubKey(newValue: String): ServerTrait                   = this.copy(pubKey = newValue)
    override def i(newValue: Int): ServerTrait                           = this.copy(i = newValue)
    override def withRound(round: Long): ServerTrait                     = this.copy(dkgRound = round)
    override def withScalar(scalar: String): ServerTrait                 = this.copy(dkgScalar = scalar)
    override def withCommit(commit: String): ServerTrait                 = this.copy(dkgCommit = commit)
    override def withShadows(shadows: String): ServerTrait               = this.copy(dkgShadows = shadows)
    override def withExponents(exponents: String): ServerTrait           = this.copy(dkgExponents = exponents)
    override def withComplaints(complaints: Vector[String]): ServerTrait = this.copy(dkgComplaints = complaints)
    override val `type`: ServerType                                      = ServerType.Main
  }

  private val serverReads: Reads[ServerTrait] = { json =>
    val commonPartReader =
      (JsPath \ "description").read[String] and
        (JsPath \ "pubKey").read[String] and
        (JsPath \ "status").readWithDefault[Status](Status.Active) and
        (JsPath \ "dkgRound").readWithDefault[Long](0) and
        (JsPath \ "dkgScalar").readWithDefault[String]("") and
        (JsPath \ "dkgCommit").readWithDefault[String]("") and
        (JsPath \ "dkgShadows").readWithDefault[String]("") and
        (JsPath \ "dkgComplaints").readWithDefault[Vector[String]](Vector.empty) and
        (JsPath \ "dkgExponents").readWithDefault[String]("") and
        (JsPath \ "i").read[Int]

    val serverType = (json \ "type").as[ServerType]
    serverType match {
      case com.wavesplatform.voting.contract.ServerType.Decrypt =>
        commonPartReader(DecryptServer.apply _).reads(json)
      case com.wavesplatform.voting.contract.ServerType.Main =>
        commonPartReader(MainServer.apply _).reads(json)
    }
  }

  def addTypeTransformer(serverType: ServerType): Reads[JsObject] = JsPath.json.update(
    (__ \ "type").json.put(JsString(serverType.entryName))
  )
  private implicit val decryptServerWrites: OWrites[DecryptServer] = {
    Json.writes[DecryptServer].transform(js => js.transform(addTypeTransformer(ServerType.Decrypt)).get)
  }
  private implicit val mainServerWrites: OWrites[MainServer] = {
    Json.writes[MainServer].transform(js => js.transform(addTypeTransformer(ServerType.Main)).get)
  }

  implicit val serverTraitWrites: OWrites[ServerTrait] = {
    case ds: DecryptServer => decryptServerWrites.writes(ds)
    case ms: MainServer    => mainServerWrites.writes(ms)
  }

  implicit val serverTraitFormat: OFormat[ServerTrait] = {
    OFormat(serverReads, serverTraitWrites)
  }
}
