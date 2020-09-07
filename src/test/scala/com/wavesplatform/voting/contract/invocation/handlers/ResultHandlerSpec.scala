package com.wavesplatform.voting.contract.invocation.handlers

import java.math.BigInteger
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.util.ContractTransactionExtension._
import com.wavesplatform.voting.contract.util.TestDateTime._
import com.wavesplatform.voting.contract.{ServerTrait, Status, VotingBase, _}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ResultHandlerSpec extends AnyFreeSpec with Matchers {
  private val round           = 2L
  private val senderPublicKey = "senderPublicKey"
  private val resultsValue    = "some results value"

  private val contractTransaction = ContractTransaction(
    id = "id",
    `type` = 104,
    sender = "sender",
    senderPublicKey = senderPublicKey,
    contractId = "contractId",
    params = Seq(
      DataEntry(key = "results", value = DataEntry.Value.StringValue(resultsValue))
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
    status = VotingStatus.Completed,
    dateStart = Instant.now().minus(2, ChronoUnit.DAYS).truncateToSeconds(),
    dateEnd = Some(Instant.now()),
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
    )
  )

  "all validations passed" in {
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = ResultsHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right
    callRes.right.get should contain theSameElementsAs Seq(
      DataEntry("RESULTS", DataEntry.Value.StringValue(resultsValue)))
  }

  "there is no 'results' in contractTransaction" in {
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val contractTransactionWithoutMainKey = contractTransaction.replaceDataEntryParam("results", DataEntry.Value.Empty)
    val callRes                           = ResultsHandler.call(contractTransactionWithoutMainKey, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.RequiredParamIsMissing("results")
  }

  "check all active servers have empty dkgComplaints for current round" in {
    val serverWithDkgComplaints =
      ServerTrait.DecryptServer(
        description = "description",
        i = 4,
        pubKey = "pubKey4",
        status = Status.Active,
        dkgRound = round,
        dkgScalar = "scalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector("some complaint"),
        dkgExponents = "[]"
      )

    val serversWithServerWithDkgComplaints = serverWithDkgComplaints +: servers
    val contractState: ContractKeysResponse =
      ContractKeysResponse(serversWithServerWithDkgComplaints.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = ResultsHandler.call(contractTransaction, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.SomeServersHaveComplaints
  }

  "blocked server have dkgComplaints, but it is fine" in {
    val serverWithDkgComplaints =
      ServerTrait.DecryptServer(
        description = "description",
        i = 4,
        pubKey = "pubKey4",
        status = Status.Blocked,
        dkgRound = round,
        dkgScalar = "scalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector("some complaint"),
        dkgExponents = "[]"
      )

    val serversWithServerWithDkgComplaints = serverWithDkgComplaints +: servers
    val contractState: ContractKeysResponse =
      ContractKeysResponse(serversWithServerWithDkgComplaints.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = ResultsHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right
  }

  "check all active servers have same dkgRound value and it equals to 'dkgRound' from contract state" - {
    "one server have wrong dkgRound" in {
      val serverWithWrongRound = ServerTrait.DecryptServer(
        description = "description",
        i = 4,
        pubKey = "pubKey4",
        status = Status.Active,
        dkgRound = 66,
        dkgScalar = "dkgScalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector.empty,
        dkgExponents = "[]"
      )
      val serversWithWrongRound = serverWithWrongRound +: servers
      val contractState: ContractKeysResponse =
        ContractKeysResponse(serversWithWrongRound.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
      val callRes = ResultsHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.ServersNotOnTheSameRound
    }

    "blocked server have wrong dkgRound" in {
      val serverWithWrongRound = ServerTrait.DecryptServer(
        description = "description",
        i = 4,
        pubKey = "pubKey4",
        status = Status.Blocked,
        dkgRound = 66,
        dkgScalar = "dkgScalar",
        dkgCommit = "dkgCommit",
        dkgShadows = "[]",
        dkgComplaints = Vector.empty,
        dkgExponents = "[]"
      )
      val serversWithWrongRound = serverWithWrongRound +: servers
      val contractState: ContractKeysResponse =
        ContractKeysResponse(serversWithWrongRound.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
      val callRes = ResultsHandler.call(contractTransaction, contractState)
      callRes shouldBe 'right
    }

    "servers dkgRound differ from contract state 'dkgRound'" in {
      val votingBaseWithWrongRaund = votingBase.copy(dkgRound = 666L)
      val contractState: ContractKeysResponse =
        ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithWrongRaund.toDataEntry))
      val callRes = ResultsHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.ServersNotOnTheSameRound
    }

  }

  "check that voting is not finished" in {
    val votingBaseWithUpdatedTime = votingBase.copy(status = VotingStatus.Active, dateEnd = None)
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithUpdatedTime.toDataEntry))
    val callRes = ResultsHandler.call(contractTransaction, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.VotingIsNotYetFinished
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

      val contractState: ContractKeysResponse = ContractKeysResponse(wrongServers.map(_.toDataEntry))
      val callRes                             = ResultsHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.SenderCantVote(senderPublicKey)
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

      val contractState: ContractKeysResponse = ContractKeysResponse(wrongServers.map(_.toDataEntry))
      val callRes                             = ResultsHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.SenderCantVote(senderPublicKey)
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

      val contractState: ContractKeysResponse = ContractKeysResponse(wrongServers.map(_.toDataEntry))
      val callRes                             = ResultsHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.SenderCantVote(senderPublicKey)
    }
  }

}
