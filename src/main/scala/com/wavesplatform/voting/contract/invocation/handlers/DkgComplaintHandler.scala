package com.wavesplatform.voting.contract.invocation.handlers

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.{
  ContractKeysResponse,
  ContractTransaction,
  TransactionExistsRequest,
  TransactionServiceClient
}
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.validators.{UpdateContractValidators, VotingValidators}
import com.wavesplatform.voting.contract.{ServerTrait, VotingError, _}

import scala.collection.Seq
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Проверки:
  *
  * Проверить что отправитель указан в ключе SERVER_<publicKey>, его тип SERVER_<publicKey>.type равен decrypt или main и SERVER_<publicKey>.status равен active
  * Проверить что обязательные поля не пустые
  * Проверить что VOTING_BASE.dateStart позже текущего времени (UTC по часам смарт-контракта) и VOTING_BASE.status равно active
  * Проверить что указанный round равен VOTING_BASE.dkgRound
  * Проверить что транзакции указанные в txId сохранены в блокчейне
  * Запись:
  *
  * Сохранить дополнить массив complaints в параметре SERVER_<publicKey>.dkgComplaints[] значениями txId указанными в транзакции
  */
class DkgComplaintHandler(transactionExistenceChecker: String => Either[VotingError, Boolean]) extends CallHandler {

  override def getContractState(
    contractTransaction: ContractTransaction,
    stateService: VotingStateService): Future[ContractKeysResponse] = {
    stateService
      .requestContractKeys(contractTransaction, Seq("VOTING_BASE", s"SERVER_${contractTransaction.senderPublicKey}"))
  }

  def call(
    contractTransaction: ContractTransaction,
    contractState: ContractKeysResponse): Either[VotingError, Seq[DataEntry]] = {
    val txParams = contractTransaction.params

    for {
      voterServer           <- VotingValidators.serverCanVote(contractTransaction, contractState)
      votingBase            <- VotingValidators.validateVotingNotStarted(contractState)
      round                 <- txParams.extractLongParam("round")
      _                     <- UpdateContractValidators.checkRoundValue(round, votingBase.dkgRound)
      contractTransactionId <- txParams.extractStringParam("txId")
      _                     <- checkTransactionExists(contractTransactionId)
      updatedServer = updateServer(voterServer, contractTransactionId)
    } yield Seq(updatedServer.toDataEntry)
  }

  private def updateServer(oldServer: ServerTrait, complaintTxId: String): ServerTrait = {
    oldServer.withComplaints(complaintTxId +: oldServer.dkgComplaints)
  }

  private def checkTransactionExists(contractTransactionId: String): Either[VotingError, Unit] = {
    for {
      transactionExists <- transactionExistenceChecker(contractTransactionId)
      _ <- Either.cond(
        transactionExists,
        (),
        VotingError.ComplaintTransactionDoesNotExist(contractTransactionId)
      )
    } yield ()
  }
}

object DkgComplaintHandlerFactory {

  def createHandler(transactionService: TransactionServiceClient, authToken: String): DkgComplaintHandler = {
    def transactionExistenceChecker(authToken: String)(txId: String): Either[VotingError, Boolean] = {
      transactionService
        .transactionExists()
        .withAuth(authToken)
        .invoke(TransactionExistsRequest(txId))
        .waitForResponse(3.seconds)
        .map(_.exists)
    }

    val txExistenceChecker = transactionExistenceChecker(authToken) _
    new DkgComplaintHandler(txExistenceChecker)
  }
}
