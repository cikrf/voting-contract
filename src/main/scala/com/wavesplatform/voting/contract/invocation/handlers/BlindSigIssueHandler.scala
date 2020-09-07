package com.wavesplatform.voting.contract.invocation.handlers

import cats.implicits._
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.common.DataEntry.Value.{IntValue, StringValue}
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.{VotingError, _}

import scala.collection.Seq
import scala.concurrent.Future

/**
  * Запись:
  *
  * Сохранить полученные значения на соответствующие ключи BLINDSIG_<userId>_<timestamp> = 1
  * Если смарт-контракт отклоняет транзакцию, сохранить на ключ FAIL_<txId>_blindSig причину отклонения транзакции
  */
object BlindSigIssueHandler extends CallHandler {

  override def getContractState(
    contractTransaction: ContractTransaction,
    stateService: VotingStateService): Future[ContractKeysResponse] = Future.successful(ContractKeysResponse())

  override def call(
    contractTransaction: ContractTransaction,
    contractState: ContractKeysResponse): Either[VotingError, Seq[DataEntry]] = {
    val txParams = contractTransaction.params
    (for {
      userIdStr <- txParams.extractStringParam("userIds")
      userIdArr <- parseJson[Vector[String]](userIdStr)
      timestamp = System.currentTimeMillis()
      result    = userIdArr.map(UserBlindSig(_, timestamp).toDataEntry)
    } yield result).recover(recoverOnFail(contractTransaction))
  }

  private def recoverOnFail(tx: ContractTransaction): PartialFunction[VotingError, Seq[DataEntry]] = {
    case e: VotingError => Seq(DataEntry(s"FAIL_${tx.id}_blindSig", StringValue(e.errorMessage)))
  }
}

private case class UserBlindSig(userId: String, timestamp: Long) {
  def toDataEntry: DataEntry = DataEntry(s"BLINDSIG_${userId}_$timestamp", IntValue(1))
}
