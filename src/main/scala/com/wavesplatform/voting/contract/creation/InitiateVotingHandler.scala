package com.wavesplatform.voting.contract.creation

import com.wavesplatform.protobuf.common.DataEntry
import com.wavesplatform.protobuf.service.ContractTransaction
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.util.InstantUtil
import com.wavesplatform.voting.contract.validators.ServerListValidator
import com.wavesplatform.voting.contract.{
  parseJson,
  toJsonStr,
  PollType,
  ServerTrait,
  VotingBase,
  VotingError,
  VotingStatus
}

import scala.collection.Seq

/**
  * Проверки:
  *
  * Проверить что обязательные поля не пустые
  * Проверить что pollType равно blind
  * Проверить что массив servers[] транзакции не пустой и publicKey отправителя транзакции соотнесен с сервером типом main, кол-во серверов в нем больше k,  кол-во серверов типа main равно 1
  * Проверить что каждому серверу из массива servers[] присвоен уникальный порядковый номер i, счет от 1 до n.
  *
  * Запись:
  *
  * Значения ключей pollId, pollType, bulletinHash, dimension, blindSigModulo, blindSigExponent, k, dateStart из транзакции сохранить на ключ VOTING_BASE (в соответствующий параметр)
  * Каждый элемент массива servers[] сохранить на отдельные ключи SERVER_<publicKey>, параметр SERVER_<publicKey>.status установить на active
  * В параметр VOTING_BASE.dkgRound сохранить значение 1, в параметр VOTING_BASE.status сохранить значение active
  */
object InitiateVotingHandler {

  def handle(tx: ContractTransaction): Either[VotingError, Seq[DataEntry]] =
    for {
      state <- extract(tx.params)
      _     <- validate(tx.senderPublicKey, state)
    } yield state.toDataEntry

  private def extract(txParams: Seq[DataEntry]): Either[VotingError, InitiateVotingState] = {
    for {
      pollId           <- txParams.extractStringParam("pollId")
      pollType         <- txParams.extractEnumParam("pollType", PollType)
      bulletinHash     <- txParams.extractStringParam("bulletinHash")
      dimensionStr     <- txParams.extractStringParam("dimension")
      dimension        <- parseJson[Vector[Int]](dimensionStr)
      blindSigModulo   <- txParams.extractBigIntegerParam("blindSigModulo")
      blindSigExponent <- txParams.extractBigIntegerParam("blindSigExponent")
      dateStart        <- txParams.extractStringParam("dateStart")
      k                <- txParams.extractLongParam("k")
      serversJsonStr   <- txParams.extractStringParam("servers")
      servers          <- parseJson[Seq[ServerTrait]](serversJsonStr)
    } yield {
      val votingBase = VotingBase(
        pollId,
        pollType,
        bulletinHash,
        dimension,
        blindSigModulo,
        blindSigExponent,
        k,
        dkgRound = 1,
        InstantUtil.parse(dateStart),
        None,
        status = VotingStatus.Active
      )

      InitiateVotingState(votingBase, servers.map(_.active))
    }
  }

  private def validate(senderPubKey: String, state: InitiateVotingState): Either[VotingError, InitiateVotingState] = {
    for {
      _ <- ServerListValidator.containsSenderPubKey(state.servers, senderPubKey)
      _ <- ServerListValidator.validate(state.servers, state.votingBase.k)
    } yield state
  }
}

case class InitiateVotingState(votingBase: VotingBase, servers: Seq[ServerTrait]) {
  def toDataEntry: Seq[DataEntry] = {
    val serversDE  = servers.map(_.toDataEntry)
    val serversKey = DataEntry("SERVERS", toJsonStr(serversDE.map(_.key)))
    Seq(votingBase.toDataEntry, serversKey) ++ serversDE
  }
}
