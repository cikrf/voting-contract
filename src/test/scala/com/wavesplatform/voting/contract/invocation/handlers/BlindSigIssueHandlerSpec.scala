package com.wavesplatform.voting.contract.invocation.handlers

import java.time.Instant

import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.common.DataEntry.Value.StringValue
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BlindSigIssueHandlerSpec extends AnyFreeSpec with Matchers {

  private val senderPublicKey = "senderPublicKey"
  private val userId          = Seq("user_1", "user_2", "user_3")

  private val contractTransaction = ContractTransaction(
    id = "id",
    `type` = 104,
    sender = "sender",
    senderPublicKey = senderPublicKey,
    contractId = "contractId",
    params = Seq(
      DataEntry(key = "userIds", value = toJsonStr(userId))
    ),
    fee = 22L,
    version = 2,
    proofs = ByteString.EMPTY,
    timestamp = Instant.now().toEpochMilli,
    feeAssetId = None,
    data = ContractTransaction.Data.Empty
  )

  private def fail(error: VotingError): DataEntry =
    DataEntry(s"FAIL_${contractTransaction.id}_blindSig", StringValue(error.errorMessage))

  "all validations passed" in {
    val contractState = ContractKeysResponse()
    val callRes       = BlindSigIssueHandler.call(contractTransaction, contractState)
    callRes shouldBe 'right

    val blingSigs = callRes.right.get
    blingSigs.size shouldBe userId.size
    blingSigs.foreach { blindSigDE => blindSigDE.key should startWith("BLINDSIG_") }
  }

  "userIds param is missing" in {
    val contractState = ContractKeysResponse()
    val callRes       = BlindSigIssueHandler.call(contractTransaction.copy(params = Seq.empty), contractState)

    callRes shouldBe 'right
    callRes.right.get shouldBe Seq(fail(VotingError.RequiredParamIsMissing("userIds")))
  }
}
