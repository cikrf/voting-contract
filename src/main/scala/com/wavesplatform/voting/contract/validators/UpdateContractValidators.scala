package com.wavesplatform.voting.contract.validators

import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.{ServerTrait, ServerType, Status, VotingError}

object UpdateContractValidators {

  def checkSenderIsActiveMainServer(tx: ContractTransaction, state: ContractKeysResponse): Either[VotingError, Unit] = {
    for {
      servers <- state.getServers
      _ <- Either.cond(
        servers.exists(isActiveMainServer(tx, _)),
        (),
        VotingError.ServersDoNotContainSenderPubKey(tx.senderPublicKey))
    } yield ()
  }

  def checkSenderIsActiveMainServer(tx: ContractTransaction, server: ServerTrait): Either[VotingError, Unit] = {
    Either.cond(isActiveMainServer(tx, server), (), VotingError.ServersDoNotContainSenderPubKey(tx.senderPublicKey))
  }

  private def isActiveMainServer(tx: ContractTransaction, server: ServerTrait): Boolean = {
    server.pubKey == tx.senderPublicKey && server.`type` == ServerType.Main && server.status == Status.Active
  }

  def checkRoundValue(current: Long, expected: Long): Either[VotingError, Unit] = {
    Either.cond(
      current == expected,
      (),
      VotingError.WrongRound(current, expected)
    )
  }
}
