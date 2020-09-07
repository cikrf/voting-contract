package com.wavesplatform.voting.contract.invocation.handlers

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.VotingError.UpdateRoundError
import com.wavesplatform.voting.contract._
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.validators.{ServerListValidator, UpdateContractValidators, VotingValidators}

import scala.collection.Seq
import scala.concurrent.Future

/**
  * Проверки:
  *
  * Проверить что отправитель указан в ключе SERVER_<publicKey>, его тип равен main
  * Проверить что обязательные поля не пустые
  * Проверить что VOTING_BASE.dateStart позже текущего времени (UTC по часам смарт-контракта) и VOTING_BASE.status равно active
  * Проверить что значение round транзакции больше значения VOTING_BASE.dkgRound
  * Проверить что массив servers[] транзакции не пустой, содержит publicKey отправителя транзакции, этот publicKey соотнесен с типом сервера main, кол-во серверов в массиве servers[] больше k,  кол-во серверов типа main равно 1
  * Проверить что каждому серверу из массива servers[] присвоен уникальный порядковый номер i, счет от 1 до n.
  * Запись:
  *
  * Сохранить значение round транзакции в ключ VOTING_BASE.dkgRound
  * Перезаписать значение ключа MAIN_KEY на пустое значение.
  * Перезаписать значение VOTING_BASE.k на значение k из транзакции.
  * Перезаписать ключи всех серверов указанных в массиве servers[] транзакции
  * Для тех серверов, которые есть в стейте контракта, но не указаны в массиве servers[] транзакции, установить SERVER_<publicKey>.status на blocked
  */
object UpdateServerListHandler extends CallHandler {

  override def getContractState(
    contractTransaction: ContractTransaction,
    stateService: VotingStateService): Future[ContractKeysResponse] = {
    stateService.requestWithServers(contractTransaction, Seq("VOTING_BASE", "SERVERS"))
  }

  override def call(
    contractTransaction: ContractTransaction,
    contractState: ContractKeysResponse): Either[VotingError, Seq[DataEntry]] = {
    val txParams = contractTransaction.params

    for {
      k                 <- txParams.extractLongParam("k")
      round             <- txParams.extractLongParam("round")
      serversJsonStr    <- txParams.extractStringParam("servers")
      servers           <- parseJson[Seq[ServerTrait]](serversJsonStr)
      _                 <- ServerListValidator.validate(servers, k)
      _                 <- VotingValidators.validateVotingNotStarted(contractState)
      _                 <- UpdateContractValidators.checkSenderIsActiveMainServer(contractTransaction, contractState)
      updatedServerList <- applyUpdateServerListOp(servers, contractState)
      updatedVotingBase <- applyUpdateVotingBase(round, k, contractState)
      removedMainKey = DataEntry("MAIN_KEY", DataEntry.Value.StringValue(""))
    } yield updatedServerList :+ updatedVotingBase.toDataEntry :+ removedMainKey
  }

  private def applyUpdateVotingBase(
    round: Long,
    k: Long,
    contractState: ContractKeysResponse): Either[VotingError, VotingBase] = {
    for {
      votingBase <- contractState.getVotingBase
      _          <- Either.cond(round > votingBase.dkgRound, (), UpdateRoundError(round, votingBase.dkgRound))
      updatedVotingBase = votingBase.copy(dkgRound = round, k = k)
    } yield updatedVotingBase
  }

  private def applyUpdateServerListOp(
    servers: Seq[ServerTrait],
    contractState: ContractKeysResponse): Either[VotingError, Seq[DataEntry]] = {
    val shouldBeDisabled = for {
      contractStateServers <- contractState.getServers
      reqServersPubKeys       = servers.map(_.pubKey)
      shouldBeDisabledServers = contractStateServers.filter(pk => !reqServersPubKeys.contains(pk.pubKey))
    } yield shouldBeDisabledServers

    shouldBeDisabled map { dis =>
      val toDisable  = dis.map(_.block)
      val toActivate = servers.map(_.active)
      val result     = (toDisable ++ toActivate).map(_.toDataEntry)
      val serversKey = DataEntry("SERVERS", toJsonStr(result.map(_.key)))
      serversKey +: result
    }
  }
}
