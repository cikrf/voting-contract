package com.wavesplatform.voting.contract.creation

import java.math.BigInteger
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.ContractTransaction
import com.wavesplatform.voting.contract.util.ContractTransactionExtension._
import com.wavesplatform.voting.contract.util.TestDateTime
import com.wavesplatform.voting.contract.util.TestDateTime._
import com.wavesplatform.voting.contract.{ServerTrait, Status, VotingBase, _}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InitiateVotingHandlerSpec extends AnyFreeSpec with Matchers {
  private val senderPublicKey = "senderPublicKey"
  private val votingBase = VotingBase(
    pollId = "pollIdValue",
    pollType = PollType.Blind,
    bulletinHash = "bulletinHashValue",
    dimension = Vector(2, 2, 2),
    status = VotingStatus.Active,
    dateStart = Instant.now().plus(1, ChronoUnit.DAYS).truncateToSeconds(),
    dateEnd = None,
    k = 2,
    dkgRound = 1,
    blindSigModulo = new BigInteger(
      "c8d503b962f15fcc18ae7ae2fba48eb472d8428ad6d496cf27520792a4fd4c7807e8549ef1563d8bf8fdf56e2669e3522ba1860cbc727c888cd2478ae4268929597dc8480ec570c1bb968191451c452e0dd4e651405ef5f0a76164d9715ac4739b7a440cdb7a58d82e5344be8a0ee2acde06c7af061cdb0c75958a755e4e6ddf8da24c1fea3886cb6aa2bfc9b2de8aee5a49250d2aee3a2e016bda701eba249819e78e9e27ac991842abe5075677d6b9eebb1a12d9614eaeaa6fdc2eefc2f0df73ff8955b7b6489c21c2cf8e116a5d2ce50a0cfb6d922d363758e35baee09b8f567a87ead72302d97ea2eb42eb61a619932d40b17e3787637028afb683a0ac99f5bdbfb0ab6f51622755ad04c05528a8aa267b3318e4f51e46c1a05afcd3392d3a7e83098317954207b44768b6da9eb4b39ee9b6b646dd9ba6256fb6a02e7aca2e52fd2a41b2edc2475e2744e417bb8d68e90a6eb3f7416892f185ae54e9251b6cdfb2368ca087d42a6bab9be57ed6dc299d778c76be0b3d2a8e186558a4689f",
      16),
    blindSigExponent = new BigInteger("10001", 16)
  )

  private val servers: Seq[ServerTrait] = Seq(
    ServerTrait.DecryptServer(
      description = "description",
      i = 1,
      pubKey = senderPublicKey,
      status = Status.Active,
      dkgRound = 0,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.MainServer(
      description = "description",
      i = 2,
      pubKey = "pubKey",
      status = Status.Active,
      dkgRound = 0,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 3,
      pubKey = "pubKey",
      status = Status.Active,
      dkgRound = 0,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 4,
      pubKey = "pubKey",
      status = Status.Active,
      dkgRound = 0,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    )
  )

  private val serversDE    = servers.map(server => server.toDataEntry)
  private val serverListDE = DataEntry("SERVERS", toJsonStr(serversDE.map(_.key)))

  private val contractTransaction = ContractTransaction(
    id = "id",
    `type` = 103,
    sender = "sender",
    senderPublicKey = senderPublicKey,
    contractId = "contractId",
    params = Seq(
      DataEntry(key = "pollId", value = DataEntry.Value.StringValue(votingBase.pollId)),
      DataEntry(key = "pollType", value = DataEntry.Value.StringValue(PollType.Blind.toString)),
      DataEntry(key = "bulletinHash", value = DataEntry.Value.StringValue(votingBase.bulletinHash)),
      DataEntry(key = "dimension", value = toJsonStr(votingBase.dimension)),
      DataEntry(key = "dateStart", value = DataEntry.Value.StringValue(TestDateTime.toString(votingBase.dateStart))),
      DataEntry(key = "servers", value = toJsonStr(servers)),
      DataEntry(key = "k", value = DataEntry.Value.IntValue(votingBase.k)),
      DataEntry(key = "blindSigModulo", value = DataEntry.Value.StringValue(votingBase.blindSigModulo.toString(16))),
      DataEntry(key = "blindSigExponent", value = DataEntry.Value.StringValue(votingBase.blindSigExponent.toString(16)))
    ),
    fee = 100000000L,
    version = 2,
    proofs = ByteString.EMPTY,
    timestamp = Instant.now().toEpochMilli,
    feeAssetId = None,
    data = ContractTransaction.Data.Empty
  )

  "validate initiateContractTransaction and extract parameters" in {
    val handleResult = InitiateVotingHandler.handle(contractTransaction)
    handleResult shouldBe 'right
    handleResult.right.get should contain theSameElementsAs Seq(votingBase.toDataEntry, serverListDE) ++ serversDE
  }

  //Проверить что массив servers[] транзакции содержит publicKey отправителя транзакции
  "one of the servers should be with senders publicKey" in {
    val invalidPubKey             = "invalidPubKey"
    val contractWithInvalidPubKey = contractTransaction.copy(senderPublicKey = invalidPubKey)
    val handleResult              = InitiateVotingHandler.handle(contractWithInvalidPubKey)
    handleResult shouldBe 'left
    handleResult.left.get shouldBe VotingError.ServersDoNotContainSenderPubKey(invalidPubKey)
  }

  //Check that servers[] contains >= servers with type `server` and exactly 1 with type `mainServer`
  "active servers count >= k and mainServer is exactly one" - {
    "not enought servers" in {
      val contractWithBiggerK = contractTransaction.replaceDataEntryParam("k", DataEntry.Value.IntValue(10))
      val handleResult        = InitiateVotingHandler.handle(contractWithBiggerK)
      handleResult shouldBe 'left
      handleResult.left.get shouldBe VotingError.InvalidServers(10)
    }

    "no mainServer" in {
      val noMainServer              = servers.filterNot(s => s.`type` == ServerType.Main)
      val contractWithoutMainServer = contractTransaction.replaceDataEntryParam("servers", toJsonStr(noMainServer))
      val handleResult              = InitiateVotingHandler.handle(contractWithoutMainServer)
      handleResult shouldBe 'left
      handleResult.left.get shouldBe VotingError.InvalidServers(votingBase.k)
    }

    "two mainServers" in {
      val mainServer                    = servers.find(s => s.`type` == ServerType.Main).get
      val serversWithSeveralMainServers = servers :+ mainServer.pubKey("pubKey2")
      val contractWithSeveralMainServers =
        contractTransaction.replaceDataEntryParam("servers", toJsonStr(serversWithSeveralMainServers))
      val handleResult = InitiateVotingHandler.handle(contractWithSeveralMainServers)
      handleResult shouldBe 'left
      handleResult.left.get shouldBe VotingError.InvalidServers(votingBase.k)
    }
  }

  "all servers have sequential 'i' values from 1" in {
    val serversWithWrongIndex = servers.map(_.i(5))
    val contractWithWrongIndexedServers =
      contractTransaction.replaceDataEntryParam("servers", toJsonStr(serversWithWrongIndex))
    val handleResult = InitiateVotingHandler.handle(contractWithWrongIndexedServers)
    handleResult shouldBe 'left
    handleResult.left.get shouldBe VotingError.ServerIndexesAreNotSequential(Seq(5, 5, 5, 5))
  }

  "parsing is case-insensitive to servers' types" in {
    val serversStr =
      """[{
        |   "i": 1,
        |   "pubKey": "senderPublicKey",
        |   "description": "description0",
        |   "type": "mAiN"
        |},
        |{
        |   "i": 2,
        |   "pubKey": "publicKey1",
        |   "description": "description1",
        |   "type": "DecRypt"
        |},
        |{
        |   "i": 3,
        |   "pubKey": "publicKey2",
        |   "description": "description2",
        |   "type": "dEcRypT"
        |}]""".stripMargin

    val contractWithRandomCasedServers = contractTransaction
      .replaceDataEntryParam("servers", DataEntry.Value.StringValue(serversStr))
      .replaceDataEntryParam("k", DataEntry.Value.IntValue(1))

    val handleResult = InitiateVotingHandler.handle(contractWithRandomCasedServers)
    handleResult shouldBe 'right
  }

  "dkgRound for servers is set to 0 by default" in {
    val serversJsonStr =
      """[{
        |   "i": 1,
        |   "pubKey": "senderPublicKey",
        |   "description": "description0",
        |   "type": "main"
        |},
        |{
        |   "i": 2,
        |   "pubKey": "publicKey1",
        |   "description": "description1",
        |   "type": "decrypt"
        |},
        |{
        |   "i": 3,
        |   "pubKey": "publicKey2",
        |   "description": "description2",
        |   "type": "decrypt"
        |}]""".stripMargin

    val serverSeqMaybe = parseJson[Seq[ServerTrait]](serversJsonStr)

    serverSeqMaybe shouldBe 'right
    val servers = serverSeqMaybe.right.get

    all(servers) should have('dkgRound (0))
    exactly(1, servers) should have('type (ServerType.Main))
  }
}
