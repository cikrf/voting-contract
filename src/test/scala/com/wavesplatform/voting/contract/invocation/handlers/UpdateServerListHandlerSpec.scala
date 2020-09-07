package com.wavesplatform.voting.contract.invocation.handlers

import java.math.BigInteger
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.common.DataEntry.Value.StringValue
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.util.ContractTransactionExtension._
import com.wavesplatform.voting.contract.util.TestDateTime._
import com.wavesplatform.voting.contract.{ServerTrait, Status, VotingBase, _}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UpdateServerListHandlerSpec extends AnyFreeSpec with Matchers {
  private val round           = 2L
  private val senderPublicKey = "senderPublicKey"
  private val k_value         = 2
  private val serversToUpdate: Seq[ServerTrait] = Seq(
    ServerTrait.MainServer(
      i = 1,
      description = "description",
      pubKey = senderPublicKey,
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "updated_dkgScalar",
      dkgCommit = "updated_dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 2,
      pubKey = "pubKey1",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "updated_dkgScalar",
      dkgCommit = "updated_dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 3,
      pubKey = "pubKey2",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "updated_dkgScalar",
      dkgCommit = "updated_dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 4,
      pubKey = "pubKey3",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "updated_dkgScalar",
      dkgCommit = "updated_dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    )
  )

  private val contractTransaction = ContractTransaction(
    id = "id",
    `type` = 104,
    sender = "sender",
    senderPublicKey = senderPublicKey,
    contractId = "contractId",
    params = Seq(
      DataEntry(key = "k", value = DataEntry.Value.IntValue(round)),
      DataEntry(key = "round", value = DataEntry.Value.IntValue(round + 1)),
      DataEntry(key = "servers", value = toJsonStr(serversToUpdate))
    ),
    fee = 22L,
    version = 2,
    proofs = ByteString.EMPTY,
    timestamp = Instant.now().toEpochMilli,
    feeAssetId = None,
    data = ContractTransaction.Data.Empty
  )

  private val votingBase = VotingBase(
    pollId = "pollIdValue",
    pollType = PollType.Blind,
    bulletinHash = "bulletinHashValue",
    dimension = Vector(2, 2, 2),
    status = VotingStatus.Active,
    dateStart = Instant.now().plus(1, ChronoUnit.DAYS).truncateToSeconds(),
    dateEnd = None,
    k = 999,
    dkgRound = round,
    blindSigModulo = new BigInteger(
      "c8d503b962f15fcc18ae7ae2fba48eb472d8428ad6d496cf27520792a4fd4c7807e8549ef1563d8bf8fdf56e2669e3522ba1860cbc727c888cd2478ae4268929597dc8480ec570c1bb968191451c452e0dd4e651405ef5f0a76164d9715ac4739b7a440cdb7a58d82e5344be8a0ee2acde06c7af061cdb0c75958a755e4e6ddf8da24c1fea3886cb6aa2bfc9b2de8aee5a49250d2aee3a2e016bda701eba249819e78e9e27ac991842abe5075677d6b9eebb1a12d9614eaeaa6fdc2eefc2f0df73ff8955b7b6489c21c2cf8e116a5d2ce50a0cfb6d922d363758e35baee09b8f567a87ead72302d97ea2eb42eb61a619932d40b17e3787637028afb683a0ac99f5bdbfb0ab6f51622755ad04c05528a8aa267b3318e4f51e46c1a05afcd3392d3a7e83098317954207b44768b6da9eb4b39ee9b6b646dd9ba6256fb6a02e7aca2e52fd2a41b2edc2475e2744e417bb8d68e90a6eb3f7416892f185ae54e9251b6cdfb2368ca087d42a6bab9be57ed6dc299d778c76be0b3d2a8e186558a4689f",
      16),
    blindSigExponent = new BigInteger("10001", 16)
  )

  private val servers: Seq[ServerTrait] = Seq(
    ServerTrait.MainServer(
      i = 1,
      description = "description",
      pubKey = senderPublicKey,
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 2,
      pubKey = "pubKey1",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 3,
      pubKey = "pubKey2",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 4,
      pubKey = "pubKey3",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 5,
      pubKey = "pubKey4",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    )
  )

  private val serversDE         = servers.map(_.toDataEntry)
  private val serversToUpdateDE = serversToUpdate.map(_.toDataEntry)

  "all validations passed" in {
    val contractState: ContractKeysResponse = ContractKeysResponse(serversDE ++ Seq(votingBase.toDataEntry))
    val callRes                             = UpdateServerListHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right

    val resultDataEntries = callRes.right.get
    val resultServers = resultDataEntries.collect {
      case DataEntry(key, StringValue(value)) if key != "VOTING_BASE" && key != "MAIN_KEY" && key != "SERVERS" =>
        parseJson[ServerTrait](value).right.get
    }
    val serverList =
      parseJson[Seq[String]](resultDataEntries.find(_.key == "SERVERS").get.value.stringValue.get).right.get

    val (bannedServers, updatedServers) = resultServers.partition(_.pubKey == "pubKey4")
    bannedServers.size shouldBe 1
    bannedServers.map(_.asInstanceOf[ServerTrait.DecryptServer]).map(_.i) should contain theSameElementsAs Seq(5)

    updatedServers should contain theSameElementsAs serversToUpdate
    serverList should contain theSameElementsAs (serversDE ++ serversToUpdateDE).map(_.key).toSet
  }

  "all new active servers have monotonic indexes from 1 to n" in {
    val serverWithWrongIndex =
      ServerTrait.DecryptServer(
        description = "description",
        i = 34,
        pubKey = "pubKey34",
        status = Status.Active,
        dkgRound = round,
        dkgScalar = "dkgScalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector.empty,
        dkgExponents = "[]"
      )
    val serversWithNonMonotonicIndexes = serverWithWrongIndex +: serversToUpdate
    val contractTransactionWithWrongServers =
      contractTransaction.replaceDataEntryParam("servers", toJsonStr(serversWithNonMonotonicIndexes))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractTransactionWithWrongServers, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.ServerIndexesAreNotSequential(Vector(1, 2, 3, 4, 34))
  }

  "all new active servers have monotonic indexes from 1 to n (last one is blocked)" in {
    val blockedServer =
      ServerTrait.DecryptServer(
        description = "description",
        i = 5,
        pubKey = "pubKey4",
        status = Status.Blocked,
        dkgRound = round,
        dkgScalar = "dkgScalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector.empty,
        dkgExponents = "[]"
      )
    val serversLastOneBlocked = blockedServer +: serversToUpdate
    val contractTransactionWithUpdatedServers =
      contractTransaction.replaceDataEntryParam("servers", toJsonStr(serversLastOneBlocked))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractTransactionWithUpdatedServers, contractState)
    callRes shouldBe 'right
  }

  "all new active servers have monotonic indexes from 1 to n (blocked in monotonic sequence is valid)" in {
    val invalidSeqServers: Seq[ServerTrait] = Seq(
      ServerTrait.DecryptServer(
        description = "description",
        i = 5,
        pubKey = "pubKey4",
        status = Status.Blocked,
        dkgRound = round,
        dkgScalar = "dkgScalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector.empty,
        dkgExponents = "[]"
      ),
      ServerTrait.DecryptServer(
        description = "description",
        i = 6,
        pubKey = "pubKey5",
        status = Status.Active,
        dkgRound = round,
        dkgScalar = "dkgScalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector.empty,
        dkgExponents = "[]"
      )
    )

    val serversWithBlockedInside = invalidSeqServers ++ serversToUpdate
    val contractTransactionWithUpdatedServers =
      contractTransaction.replaceDataEntryParam("servers", toJsonStr(serversWithBlockedInside))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractTransactionWithUpdatedServers, contractState)
    callRes shouldBe 'right
  }

  "new servers list is not empty" in {
    val contractWithEmptyServers =
      contractTransaction.replaceDataEntryParam("servers", DataEntry.Value.StringValue("[]"))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractWithEmptyServers, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.InvalidServers(k_value)
  }

  "new active servers count is bigger than 'k'" in {
    val contractTransactionWithBiggerK = contractTransaction.replaceDataEntryParam("k", DataEntry.Value.IntValue(666L))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractTransactionWithBiggerK, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.InvalidServers(666)
  }

  "mainServer is exactly one (when zero case)" in {
    val serversWithoutMain         = serversToUpdate.filterNot(_.`type` == ServerType.Main)
    val contractWithUpdatedServers = contractTransaction.replaceDataEntryParam("servers", toJsonStr(serversWithoutMain))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractWithUpdatedServers, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.InvalidServers(k_value)
  }

  "mainServer is exactly one (when bigger than one case)" in {
    val secondMainServer = ServerTrait.MainServer(
      i = 1,
      description = "description",
      pubKey = "pubKey42",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    )
    val serversWithTwoMain         = secondMainServer +: serversToUpdate
    val contractWithUpdatedServers = contractTransaction.replaceDataEntryParam("servers", toJsonStr(serversWithTwoMain))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractWithUpdatedServers, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.InvalidServers(k_value)
  }

  "check that voting is not started yet" in {
    val votingBaseWithUpdatedTime = votingBase.copy(dateStart = Instant.now().minus(1, ChronoUnit.DAYS))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithUpdatedTime.toDataEntry))
    val callRes = UpdateServerListHandler.call(contractTransaction, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.VotingAlreadyStarted
  }

  "checking SERVER_<publicKey>.status is active and 'type' is mainServer" - {
    "SERVER_<publicKey> is not mainServer" in {
      val wrongServers: Seq[ServerTrait] = Seq(
        ServerTrait.DecryptServer(
          description = "description",
          i = 1,
          pubKey = senderPublicKey,
          status = Status.Active,
          dkgRound = round,
          dkgScalar = "dkgScalar",
          dkgCommit = "dkgCommit",
          dkgShadows = "[]",
          dkgComplaints = Vector.empty,
          dkgExponents = "[]"
        ))

      val contractState: ContractKeysResponse =
        ContractKeysResponse(wrongServers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
      val callRes = UpdateServerListHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.ServersDoNotContainSenderPubKey(senderPublicKey)
    }

    "SERVER_<publicKey>.status is not Active" in {
      val wrongServers: Seq[ServerTrait] = Seq(
        ServerTrait.MainServer(
          i = 1,
          description = "description",
          pubKey = senderPublicKey,
          status = Status.Blocked,
          dkgRound = round,
          dkgScalar = "dkgScalar",
          dkgCommit = "dkgCommit",
          dkgShadows = "[]",
          dkgComplaints = Vector.empty,
          dkgExponents = "[]"
        ))

      val contractState: ContractKeysResponse =
        ContractKeysResponse(wrongServers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
      val callRes = UpdateServerListHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.ServersDoNotContainSenderPubKey(senderPublicKey)
    }

    "there is no SERVER_<publicKey>" in {
      val wrongServers: Seq[ServerTrait] = Seq(
        ServerTrait.MainServer(
          i = 1,
          description = "description",
          pubKey = "pubKey1",
          status = Status.Active,
          dkgRound = round,
          dkgScalar = "dkgScalar",
          dkgCommit = "dkgCommit",
          dkgShadows = "[]",
          dkgComplaints = Vector.empty,
          dkgExponents = "[]"
        ),
        ServerTrait.DecryptServer(
          description = "description",
          i = 2,
          pubKey = "pubKey2",
          status = Status.Active,
          dkgRound = round,
          dkgScalar = "dkgScalar",
          dkgCommit = "dkgCommit",
          dkgShadows = "[]",
          dkgComplaints = Vector.empty,
          dkgExponents = "[]"
        )
      )

      val contractState: ContractKeysResponse =
        ContractKeysResponse(wrongServers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
      val callRes = UpdateServerListHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.ServersDoNotContainSenderPubKey(senderPublicKey)
    }
  }
}
