package com.wavesplatform.voting.contract.invocation.handlers

import java.math.BigInteger
import java.security.Security
import java.security.interfaces.RSAPublicKey
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.common.DataEntry.Value.StringValue
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.QuestionVoteJsonSpec.QuestionVoteJsonExample
import com.wavesplatform.voting.contract.util.TestDateTime._
import com.wavesplatform.voting.contract.util.TestKeysGenerator
import com.wavesplatform.voting.contract.validators.BlindSignatureVerifier.BlindSigPublicKey
import com.wavesplatform.voting.contract.{ServerTrait, Status, VotingBase, _}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class VoteHandlerSpec extends AnyFreeSpec with Matchers {

  Security.addProvider(new BouncyCastleProvider)

  private val round = 2L
  private val senderPublicKey =
    "3fFhTr9uaB87VC1RJJzNu2vo3VYmAN8TrktJFFKLkhZTTbb14khvQ1avowpvAFTNZEjK7rqFH8jaxaGG4TVtcBhE"

  private val votes = Json.parse(s"""
                                    |[
                                    |  $QuestionVoteJsonExample,
                                    |  $QuestionVoteJsonExample
                                    |]
                                    |""".stripMargin)

  private val dimension = Vector(3, 3)

  private val keys       = TestKeysGenerator.generateKeys()
  private val blindSigPK = BlindSigPublicKey(keys.getPublic.asInstanceOf[RSAPublicKey])
  private val blindSig   = TestKeysGenerator.makeBlindSignature(keys, senderPublicKey)

  private val contractTransaction = ContractTransaction(
    id = "id",
    `type` = 104,
    sender = "sender",
    senderPublicKey = senderPublicKey,
    contractId = "contractId",
    params = Seq(
      DataEntry("vote", toJsonStr(votes)),
      DataEntry("blindSig", StringValue(blindSig.toString(16)))
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
    dimension = dimension,
    status = VotingStatus.Active,
    dateStart = Instant.now().minus(1, ChronoUnit.DAYS).truncateToSeconds(),
    dateEnd = None,
    k = 999,
    dkgRound = round,
    blindSigModulo = blindSigPK.modulo,
    blindSigExponent = blindSigPK.exp
  )

  private val servers: Seq[ServerTrait] = Seq(
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
    ),
    ServerTrait.DecryptServer(
      description = "description",
      i = 2,
      pubKey = "pubKey",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector.empty,
      dkgExponents = "[]"
    )
  )

  private def fail(error: VotingError): DataEntry =
    DataEntry(
      s"FAIL_${contractTransaction.sender}_${contractTransaction.id}_vote",
      toJsonStr(FailReason(error.errorMessage, senderPublicKey)))

  "all validations passed" in {
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = VoteHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right
    callRes.right.get should contain theSameElementsAs Seq(
      VoteWrapper(contractTransaction.id, blindSig).toDataEntry(contractTransaction.senderPublicKey))
  }

  "blind signature verification failed" in {
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))

    val invalidBlindSig = blindSig.add(BigInteger.TWO)
    val invalidTx = contractTransaction.copy(params = Seq(
      DataEntry("vote", toJsonStr(votes)),
      DataEntry("blindSig", StringValue(invalidBlindSig.toString(16)))
    ))

    val callRes = VoteHandler.call(invalidTx, contractState)
    callRes shouldBe 'right
    callRes.right.get shouldBe Seq(fail(VotingError.InvalidBlindSig(invalidBlindSig)))
  }

  "dimension violation check failed (for questions)" in {
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.copy(dimension = Vector(3)).toDataEntry))

    val callRes = VoteHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right
    callRes.right.get shouldBe Seq(fail(VotingError.QuestionDimensionViolation(2, 1)))
  }

  "dimension violation check failed (for question options)" in {
    val contractState: ContractKeysResponse =
      ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBase.copy(dimension = Vector(4, 4)).toDataEntry))

    val callRes = VoteHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right
    callRes.right.get shouldBe Seq(fail(VotingError.QuestionOptionsDimensionViolation(3, 4)))
  }

  "check server complaints emptiness" in {
    val serverWithComplaints = ServerTrait.DecryptServer(
      description = "description",
      i = 3,
      pubKey = "pubKey",
      status = Status.Active,
      dkgRound = round,
      dkgScalar = "dkgScalar",
      dkgCommit = "dkgCommit",
      dkgShadows = "[]",
      dkgComplaints = Vector("complaint value"),
      dkgExponents = "[]"
    )
    val serversWithComplaints = serverWithComplaints +: servers
    val contractState: ContractKeysResponse =
      ContractKeysResponse(serversWithComplaints.map(_.toDataEntry) ++ Seq(votingBase.toDataEntry))
    val callRes = VoteHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right
    callRes.right.get shouldBe Seq(fail(VotingError.SomeServersHaveComplaints))
  }

  "check all servers have same dkgRound value and it equals to 'dkgRound' from contract state" - {
    "one server have wrong dkgRound" in {
      val serverWithWrongRound = ServerTrait.DecryptServer(
        description = "description",
        i = 3,
        pubKey = "pubKey",
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
      val callRes = VoteHandler.call(contractTransaction, contractState)
      callRes shouldBe 'right
      callRes.right.get shouldBe Seq(fail(VotingError.ServersNotOnTheSameRound))
    }

    "servers dkgRound differ from contract state 'dkgRound'" in {
      val votingBaseWithWrongDkgRound = votingBase.copy(dkgRound = 666L)
      val contractState: ContractKeysResponse =
        ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithWrongDkgRound.toDataEntry))
      val callRes = VoteHandler.call(contractTransaction, contractState)
      callRes shouldBe 'right
      callRes.right.get shouldBe Seq(fail(VotingError.ServersNotOnTheSameRound))
    }
  }

  "voting is in progress" - {
    "voting not started yet" in {
      val votingBaseWithUpdatedTime = votingBase.copy(dateStart = Instant.now().plus(1, ChronoUnit.DAYS))
      val contractState: ContractKeysResponse =
        ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithUpdatedTime.toDataEntry))
      val callRes = VoteHandler.call(contractTransaction, contractState)
      callRes shouldBe 'right
      callRes.right.get shouldBe Seq(fail(VotingError.VotingIsNotInProgress))
    }

    "voting already ended" in {
      val votingBaseWithUpdatedTime =
        votingBase.copy(status = VotingStatus.Completed, dateEnd = Some(Instant.now().minus(1, ChronoUnit.DAYS)))
      val contractState: ContractKeysResponse =
        ContractKeysResponse(servers.map(_.toDataEntry) ++ Seq(votingBaseWithUpdatedTime.toDataEntry))
      val callRes = VoteHandler.call(contractTransaction, contractState)
      callRes shouldBe 'right
      callRes.right.get shouldBe Seq(fail(VotingError.VotingIsNotInProgress))
    }
  }

}
