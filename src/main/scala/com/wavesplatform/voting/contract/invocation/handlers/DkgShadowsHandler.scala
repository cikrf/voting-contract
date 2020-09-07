package com.wavesplatform.voting.contract.invocation.handlers

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.validators.{UpdateContractValidators, VotingValidators}
import com.wavesplatform.voting.contract.{VotingBase, VotingError, _}

import scala.collection.Seq
import scala.concurrent.Future

/**
  * Проверки:
  *
  * Проверить что отправитель указан в ключе SERVER_<publicKey>, его тип SERVER_<publicKey>.type равен decrypt или main и SERVER_<publicKey>.status равен active
  * Проверить что обязательные поля не пустые
  * Проверить что VOTING_BASE.dateStart позже текущего времени (UTC по часам смарт-контракта) и VOTING_BASE.status равно active
  * Проверить что указанный round равен VOTING_BASE.dkgRound и SERVER_<publicKey>.dkgShadows пустое (потерли при перезапуске раунда и не записывали ранее)
  * Проверить что каждый активный сервер типа decrypt или main сохранил SERVER_<publicKey>.dkgScalar для текущего раунда
  * Запись:
  *
  * Сохранить строковый параметр (массив) shadows в параметр SERVER_<publicKey>.dkgShadows
  * Сохранить строковый параметр (массив) exponents в параметр SERVER_<publicKey>.dkgExponents
  */
object DkgShadowsHandler extends CallHandler {

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
      voterServer <- VotingValidators.serverCanVote(contractTransaction, contractState)
      votingBase  <- VotingValidators.validateVotingNotStarted(contractState)
      round       <- txParams.extractLongParam("round")
      _           <- UpdateContractValidators.checkRoundValue(round, votingBase.dkgRound)
      _           <- checkDkgShadowsEmptiness(voterServer)
      _           <- checkAllServersPublishScalar(contractState, votingBase)
      shadows     <- txParams.extractStringParam("shadows")
      exponents   <- txParams.extractStringParam("exponents")
      updatedServer = voterServer.withShadows(shadows).withExponents(exponents)
    } yield Seq(updatedServer.toDataEntry)
  }

  private def checkAllServersPublishScalar(
    contractState: ContractKeysResponse,
    votingBase: VotingBase): Either[VotingError, Unit] = {
    for {
      servers <- contractState.getServers
      activeServers = servers.filter(_.status == Status.Active)
      _ <- Either.cond(
        activeServers.forall(s => s.dkgRound == votingBase.dkgRound && s.dkgScalar.nonEmpty),
        (),
        VotingError.NotAllServersHavePublishedScalar
      )
    } yield ()
  }

  private def checkDkgShadowsEmptiness(server: ServerTrait): Either[VotingError, Unit] = {
    Either.cond(
      server.dkgShadows.isEmpty || server.dkgShadows == "[]",
      (),
      VotingError.DkgShadowsNonEmpty(server)
    )
  }
}
