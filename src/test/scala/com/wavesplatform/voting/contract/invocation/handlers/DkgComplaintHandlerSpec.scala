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

class DkgComplaintHandlerSpec extends AnyFreeSpec with Matchers {

  private val dkgComplaintHandler: DkgComplaintHandler = {
    new DkgComplaintHandler(_ => Right(true))
  }

  private val round                  = 2L
  private val senderPublicKey        = "senderPublicKey"
  private val complaintTransactionId = "complaint transaction id"

  private val contractTransaction = ContractTransaction(
    id = "id",
    `type` = 104,
    sender = "sender",
    senderPublicKey = senderPublicKey,
    contractId = "contractId",
    params = Seq(
      DataEntry(key = "round", value = DataEntry.Value.IntValue(round)),
      DataEntry(key = "txId", value = DataEntry.Value.StringValue(complaintTransactionId))
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
      pubKey = "mainServer_publicKey",
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
    val callRes = dkgComplaintHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right

    val voterServer        = servers.find(_.pubKey == senderPublicKey).get.asInstanceOf[ServerTrait.DecryptServer]
    val updatedVoterServer = voterServer.copy(dkgComplaints = complaintTransactionId +: voterServer.dkgComplaints)
    callRes.right.get should contain theSameElementsAs Seq(updatedVoterServer.toDataEntry)
  }

  "complaint transaction not exists" in {
    val customDkgComplaintHandler = new DkgComplaintHandler(_ => Right(false))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = customDkgComplaintHandler.call(contractTransaction, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.ComplaintTransactionDoesNotExist(complaintTransactionId)
  }

  "there is no 'txId' in contractTransaction" in {
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val contractTransactionWithoutTxId = contractTransaction.replaceDataEntryParam("txId", DataEntry.Value.Empty)
    val callRes                        = dkgComplaintHandler.call(contractTransactionWithoutTxId, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.RequiredParamIsMissing("txId")
  }

  "VOTING_BASE.dkgRound == 'round' from contract state" in {
    val votingBaseWithWrongRaund = votingBase.copy(dkgRound = 666L)
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithWrongRaund.toDataEntry))
    val callRes = dkgComplaintHandler.call(contractTransaction, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.WrongRound(round, 666L)
  }

  "ckeck that voting is not started" in {
    val votingBaseWithUpdatedTime = votingBase.copy(dateStart = Instant.now().minus(1, ChronoUnit.DAYS))
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithUpdatedTime.toDataEntry))
    val callRes = dkgComplaintHandler.call(contractTransaction, contractState)
    callRes shouldBe 'left
    callRes.left.get shouldBe VotingError.VotingAlreadyStarted
  }

  "checking SERVER_<publicKey>.status is active" - {
    "SERVER_<publicKey>.status is not Active" in {
      val wrongServers: Seq[ServerTrait] = Seq(
        ServerTrait.DecryptServer(
          description = "description",
          i = 1,
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
      val callRes                             = dkgComplaintHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.SenderCantVote(senderPublicKey)
    }

    "there is no SERVER_<publicKey>" in {
      val wrongServers: Seq[ServerTrait] = Seq(
        ServerTrait.DecryptServer(
          description = "description",
          i = 1,
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
      val callRes                             = dkgComplaintHandler.call(contractTransaction, contractState)
      callRes shouldBe 'left
      callRes.left.get shouldBe VotingError.SenderCantVote(senderPublicKey)
    }
  }

}
