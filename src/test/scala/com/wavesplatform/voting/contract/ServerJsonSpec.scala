package com.wavesplatform.voting.contract

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ServerJsonSpec extends AnyFreeSpec with Matchers {

  private val server: ServerTrait = ServerTrait.DecryptServer(
    description = "description",
    i = 1,
    pubKey = "pubKey",
    status = Status.Blocked,
    dkgRound = 0,
    dkgScalar = "dkgScalar",
    dkgCommit = "dkgCommit",
    dkgShadows = "[]",
    dkgComplaints = Vector("comp1", "comp2"),
    dkgExponents = "[]"
  )
  private val mainServer: ServerTrait = ServerTrait.MainServer(
    description = "description",
    i = 2,
    pubKey = "pubKey",
    status = Status.Blocked,
    dkgRound = 0,
    dkgScalar = "dkgScalar",
    dkgCommit = "dkgCommit",
    dkgShadows = "[]",
    dkgComplaints = Vector("comp1", "comp2"),
    dkgExponents = "[]"
  )

  "ServerType.Server json read/write" in {
    val serverJsValue = Json.toJson(server)

    val fromJson = Json.fromJson[ServerTrait](serverJsValue)
    fromJson.isSuccess shouldBe true
    val parsedServer = fromJson.get
    parsedServer shouldEqual server
  }

  "ServerType.Server json read json without Status field" in {
    val serverJsObject = Json.toJsObject(server)
    val withoutStatus  = serverJsObject - "status"

    val fromJson = Json.fromJson[ServerTrait](withoutStatus)
    fromJson.isSuccess shouldBe true
    val parsedServer = fromJson.get
    parsedServer shouldEqual server.active
  }

  "ServerType.MainServer json read/write" in {
    val serverJsValue = Json.toJson(mainServer)

    val fromJson = Json.fromJson[ServerTrait](serverJsValue)
    fromJson.isSuccess shouldBe true
    val parsedServer = fromJson.get
    parsedServer shouldEqual mainServer
  }

  "ServerType.MainServer json read json without Status field" in {
    val serverJsObject = Json.toJsObject(mainServer)
    val withoutStatus  = serverJsObject - "status"

    val fromJson = Json.fromJson[ServerTrait](withoutStatus)
    fromJson.isSuccess shouldBe true
    val parsedServer = fromJson.get
    parsedServer shouldEqual mainServer.active
  }
}
