package com.wavesplatform.voting.contract.invocation.handlers

import java.math.BigInteger

import cats.implicits._
import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.VotingError.{QuestionDimensionViolation, QuestionOptionsDimensionViolation, _}
import com.wavesplatform.voting.contract.invocation.{CallHandler, VotingStateService}
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.util.DataEntriesExtension.DataEntriesExtensionMethods
import com.wavesplatform.voting.contract.validators.BlindSignatureVerifier.BlindSigPublicKey
import com.wavesplatform.voting.contract.validators.{BlindSignatureVerifier, VotingValidators}
import com.wavesplatform.voting.contract.{VotingError, _}

import scala.collection.Seq
import scala.concurrent.Future

/**
  * Проверки:
  *
  * Если на стейте контракта уже есть ключ VOTE_<publicKey>, проверить что VOTE_<publicKey>.blindSig на стейте совпадает с blindSig транзакции
  * Проверить что обязательные поля не пустые
  * Проверить корректность слепой подписи
  * Проверить что размерность бюллетеня совпадает с VOTING_BASE.dimension
  * Проверить что VOTING_BASE.dateStart позже текущего времени (UTC по часам смарт-контракта) и VOTING_BASE.status равно active
  * Проверить что для всех активных серверов значение SERVER_<publicKey>.dkgRound  одинаковое и равно VOTING_BASE.round (все на актуальном раунде)
  * Проверить что для всех активных серверов SERVER_<publicKey>.dkgComplaints[] пустое (нет жалоб)
  *
  * Запись:
  *
  * Сохранить id транзакции на ключ VOTE_<publicKey>.vote, ключ blindSig транзакции на ключ VOTE_<publicKey>.blindSig
  * Если смарт-контракт отклоняет транзакцию, сохранить на ключ FAIL_<senderAddress>_<txId>_vote публичный ключ отправителя и причину отклонения транзакции
  */
object VoteHandler extends CallHandler {

  val KeySize = 4096

  override def getContractState(
    contractTransaction: ContractTransaction,
    stateService: VotingStateService): Future[ContractKeysResponse] = {
    stateService.requestWithServers(
      contractTransaction,
      Seq("VOTING_BASE", "SERVERS", s"VOTE_${contractTransaction.senderPublicKey}"))
  }

  override def call(
    contractTransaction: ContractTransaction,
    contractState: ContractKeysResponse): Either[VotingError, Seq[DataEntry]] = {
    val senderPK = contractTransaction.senderPublicKey
    (for {
      votingBase  <- VotingValidators.validateVotingIsInProgress(contractState)
      servers     <- contractState.getServers
      _           <- VotingValidators.checkActiveServersRound(votingBase.dkgRound, servers)
      _           <- VotingValidators.checkActiveServersComplaintsEmptiness(servers)
      _           <- checkQuestionsVotes(contractState, contractTransaction)
      voteWrapper <- handleVote(contractState, votingBase, contractTransaction)
      result = Seq(voteWrapper.toDataEntry(senderPK))
    } yield result).recover(recoverOnFail(contractTransaction))
  }

  private def recoverOnFail(tx: ContractTransaction): PartialFunction[VotingError, Seq[DataEntry]] = {
    case e: VotingError =>
      val failReason = FailReason(e.errorMessage, tx.senderPublicKey)
      Seq(DataEntry(s"FAIL_${tx.sender}_${tx.id}_vote", toJsonStr(failReason)))
  }

  private def handleVote(
    contractState: ContractKeysResponse,
    votingBase: VotingBase,
    contractTransaction: ContractTransaction): Either[VotingError, VoteWrapper] = {
    val senderPK = contractTransaction.senderPublicKey
    for {
      blindSig <- contractTransaction.params.extractBigIntegerParam("blindSig")
      voteOpt  <- contractState.getVote(senderPK)
      isNew = voteOpt.isEmpty
      _ <- if (isNew) {
        validateBlindSignature(blindSig, votingBase, senderPK)
      } else {
        Either.cond(voteOpt.exists(_.blindSig == blindSig), (), BlindSigIsNotEqual(voteOpt.get.blindSig, blindSig))
      }
    } yield VoteWrapper(contractTransaction.id, blindSig)
  }

  private def validateBlindSignature(
    blindSig: BigInteger,
    votingBase: VotingBase,
    senderPublicKey: String): Either[VotingError, Unit] = {
    val publicKey = BlindSigPublicKey(votingBase.blindSigModulo, votingBase.blindSigExponent)
    Either
      .cond(BlindSignatureVerifier.verify(blindSig, publicKey, senderPublicKey, KeySize), (), InvalidBlindSig(blindSig))
  }

  private def checkQuestionsVotes(
    contractState: ContractKeysResponse,
    contractTransaction: ContractTransaction): Either[VotingError, Unit] = {
    for {
      votingBase <- contractState.getVotingBase
      dimension = votingBase.dimension
      votesJson <- contractTransaction.params.extractStringParam("vote")
      votes     <- parseJson[Vector[QuestionVote]](votesJson)
      _         <- Either.cond(votes.length == dimension.length, (), QuestionDimensionViolation(votes.length, dimension.length))
      _         <- checkSelectedOptions(dimension, votes)
    } yield ()
  }

  private def checkSelectedOptions(dimension: Vector[Int], votes: Vector[QuestionVote]): Either[VotingError, Unit] = {
    (dimension zip votes).toList.traverse {
      case (optionsCount, vote) =>
        val selectedOptions = vote.selectedOptions
        Either.cond(
          selectedOptions.length == optionsCount,
          (),
          QuestionOptionsDimensionViolation(selectedOptions.length, optionsCount))
    }.map(_ => ())
  }
}
