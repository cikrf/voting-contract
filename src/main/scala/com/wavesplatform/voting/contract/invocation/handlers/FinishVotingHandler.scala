package com.wavesplatform.voting.contract.invocation.handlers

import java.time.Instant

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.validators.UpdateContractValidators
import com.wavesplatform.voting.contract.{VotingBase, VotingError, VotingStatus}

import scala.concurrent.Future

/**
  * Проверки:
  *
  * Проверить что статус отправителя в ключе SERVER_<publicKey>, его тип SERVER_<publicKey>.type равен main
  * Проверить что VOTING_BASE.status равно active
  *
  * Запись:
  *
  * Если VOTING_BASE.dateStart позже текущего времени (UTC по часам смарт-контракта) установить VOTING_BASE.status в значение halted
  * Если VOTING_BASE.dateStart ранее текущего времени (UTC по часам смарт-контракта) установить VOTING_BASE.status в значение completed
  * Сохранить текужее время в параметр VOTING_BASE.dateEnd
  */
object FinishVotingHandler extends CallHandler {

  override def getContractState(
    contractTransaction: ContractTransaction,
    stateService: VotingStateService): Future[ContractKeysResponse] = {
    stateService
      .requestContractKeys(contractTransaction, Seq("VOTING_BASE", s"SERVER_${contractTransaction.senderPublicKey}"))
  }

  override def call(
    contractTransaction: ContractTransaction,
    contractState: ContractKeysResponse): Either[VotingError, Seq[DataEntry]] = {
    for {
      server <- contractState.getServer(contractTransaction.senderPublicKey)
      _      <- UpdateContractValidators.checkSenderIsActiveMainServer(contractTransaction, server)
      voting <- contractState.getVotingBase
      updatedVoting = handleVoting(voting)
    } yield Seq(updatedVoting.toDataEntry)
  }

  private def handleVoting(voting: VotingBase): VotingBase = {
    val now    = Instant.now()
    val status = if (voting.dateStart.isAfter(now)) VotingStatus.Halted else VotingStatus.Completed
    voting.copy(status = status, dateEnd = Some(now))
  }
}
