package com.wavesplatform.voting.contract.invocation.handlers

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service._
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.validators.{UpdateContractValidators, VotingValidators}
import com.wavesplatform.voting.contract.{VotingError, _}

import scala.collection.Seq
import scala.concurrent.Future

/**
  * Проверки:
  *
  * Проверить что отправитель указан в ключе SERVER_<publicKey>, его тип SERVER_<publicKey>.type равен decrypt или main и SERVER_<publicKey>.status равен active
  * Проверить что обязательные поля не пустые
  * Проверить что VOTING_BASE.dateStart позже текущего времени (UTC по часам смарт-контракта) и VOTING_BASE.status равно active
  * Проверить что указанный round равен VOTING_BASE.dkgRound и SERVER_<publicKey>.dkgRound < round  (сервер голосует на актуальном раунде и не голосовал в него ранее)
  * Запись:
  *
  * Сохранить значение commit в параметр SERVER_<publicKey>.dkgCommit
  * Сохранить значение round в параметр SERVER_<publicKey>.dkgRound
  * В случае, если round не равен 1, удалить параметры dkgScalar, dkgShadows и dkgComplaints для этого SERVER_<publicKey> (удаляем голоса предыдущего раунда)
  */
object DkgCommitHandler extends CallHandler {

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
      voterServer <- VotingValidators.serverCanVote(contractTransaction, contractState)
      votingBase  <- VotingValidators.validateVotingNotStarted(contractState)
      round       <- txParams.extractLongParam("round")
      _           <- UpdateContractValidators.checkRoundValue(round, votingBase.dkgRound)
      _           <- checkServerAlreadyVoting(round, voterServer)
      commit      <- txParams.extractStringParam("commit")
      updatedServer = updateServer(voterServer, commit, round)
    } yield Seq(updatedServer.toDataEntry)
  }

  private def updateServer(oldServer: ServerTrait, commit: String, round: Long): ServerTrait = {
    val firstUpdate = oldServer.withRound(round).withCommit(commit)

    val secondUpdate = if (round != 1) {
      firstUpdate.withScalar("").withShadows("").withComplaints(Vector.empty)
    } else {
      firstUpdate
    }

    secondUpdate
  }

  private def checkServerAlreadyVoting(round: Long, voterServer: ServerTrait): Either[VotingError, Unit] = {
    Either.cond(voterServer.dkgRound < round, (), VotingError.ServerAlreadyVoted(round, voterServer))
  }
}
