package com.wavesplatform.voting.contract.invocation.handlers

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service._
import com.wavesplatform.voting.contract.VotingError
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.validators.{UpdateContractValidators, VotingValidators}

import scala.concurrent.Future

/**
  * Проверки:
  *
  * Проверить что отправитель указан в ключе SERVER_<publicKey>, его тип SERVER_<publicKey>.type равен main и SERVER_<publicKey>.status равен active
  * Проверить что обязательные поля не пустые
  * Проверить что VOTING_BASE.dateStart позже текущего времени (UTC по часам смарт-контракта) и VOTING_BASE.status равно active
  * Проверить что указанный round равен VOTING_BASE.dkgRound
  * Проверить что для всех активных серверов значение SERVER_<publicKey>.dkgRound  одинаковое и равно round (все на актуальном раунде)
  * Проверить что для всех активных серверов SERVER_<publicKey>.dkgComplaints[] пустое (нет жалоб)
  *
  * Запись:
  *
  * Сохранить значение mainKey в ключ MAIN_KEY стейта контракта
  * Сохранить значение commissionKey в ключ COMMISSION_KEY стейта контракта
  * Сохранить значение dkgKey в ключ DKG_KEY стейта контракта
  */
object AddMainKeyHandler extends CallHandler {

  override def getContractState(
    contractTransaction: ContractTransaction,
    stateService: VotingStateService): Future[ContractKeysResponse] = {
    stateService.requestWithServers(contractTransaction, Seq("VOTING_BASE", "SERVERS"))
  }

  def call(
    contractTransaction: ContractTransaction,
    contractState: ContractKeysResponse): Either[VotingError, Seq[DataEntry]] = {
    val txParams = contractTransaction.params

    for {
      servers    <- contractState.getServers
      _          <- VotingValidators.mainServerCanVote(contractTransaction, servers)
      votingBase <- VotingValidators.validateVotingNotStarted(contractState)
      round      <- txParams.extractLongParam("round")
      _          <- UpdateContractValidators.checkRoundValue(round, votingBase.dkgRound)
      _          <- VotingValidators.checkActiveServersRound(round, servers)
      _          <- VotingValidators.checkActiveServersComplaintsEmptiness(servers)
      mainKey    <- txParams.extractStringParam("mainKey")
      commKey    <- txParams.extractStringParam("commissionKey")
      dkgKey     <- txParams.extractStringParam("dkgKey")
    } yield Seq(
      DataEntry("MAIN_KEY", DataEntry.Value.StringValue(mainKey)),
      DataEntry("COMMISSION_KEY", DataEntry.Value.StringValue(commKey)),
      DataEntry("DKG_KEY", DataEntry.Value.StringValue(dkgKey))
    )
  }
}
