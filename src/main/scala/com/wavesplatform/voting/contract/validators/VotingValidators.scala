package com.wavesplatform.voting.contract.validators

import java.time.Instant

import com.wavesplatform.protobuf.service.{ContractKeysResponse, ContractTransaction}
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.{ServerTrait, ServerType, Status, VotingBase, VotingError, VotingStatus}

object VotingValidators {

  def serverCanVote(tx: ContractTransaction, state: ContractKeysResponse): Either[VotingError, ServerTrait] = {
    state.getServers.flatMap(serverCanVote(tx, _))
  }

  def serverCanVote(tx: ContractTransaction, servers: Iterable[ServerTrait]): Either[VotingError, ServerTrait] = {
    val voterServerOpt = servers.find(_.pubKey == tx.senderPublicKey)
    Either.cond(
      voterServerOpt.exists(_.status == Status.Active),
      voterServerOpt.get,
      VotingError.SenderCantVote(tx.senderPublicKey))
  }

  def mainServerCanVote(
    tx: ContractTransaction,
    state: ContractKeysResponse): Either[VotingError, ServerTrait.MainServer] = {
    state.getServers.flatMap(mainServerCanVote(tx, _))
  }

  def mainServerCanVote(
    tx: ContractTransaction,
    servers: Iterable[ServerTrait]): Either[VotingError, ServerTrait.MainServer] = {
    val voterServerOpt = servers.find(_.pubKey == tx.senderPublicKey)
    Either.cond(
      voterServerOpt.exists(server => server.status == Status.Active && server.`type` == ServerType.Main),
      voterServerOpt.get.asInstanceOf[ServerTrait.MainServer],
      VotingError.SenderCantVote(tx.senderPublicKey)
    )
  }

  def checkActiveServersRound(currentRound: Long, servers: Seq[ServerTrait]): Either[VotingError, Unit] = {
    val activeServers = servers.filter(_.status == Status.Active)
    Either.cond(activeServers.forall(_.dkgRound == currentRound), (), VotingError.ServersNotOnTheSameRound)
  }

  def checkActiveServersComplaintsEmptiness(servers: Seq[ServerTrait]): Either[VotingError, Unit] = {
    val activeServers = servers.filter(_.status == Status.Active)
    Either.cond(activeServers.forall(_.dkgComplaints.isEmpty), (), VotingError.SomeServersHaveComplaints)
  }

  def validateVotingIsInProgress(state: ContractKeysResponse): Either[VotingError, VotingBase] = {
    val now = Instant.now()
    for {
      votingBase <- state.getVotingBase
      _ <- Either.cond(
        votingBase.dateStart.isBefore(now) && votingBase.status == VotingStatus.Active,
        (),
        VotingError.VotingIsNotInProgress)
    } yield votingBase
  }

  def validateVotingNotStarted(state: ContractKeysResponse): Either[VotingError, VotingBase] = {
    val now = Instant.now()
    for {
      votingBase <- state.getVotingBase
      _ <- Either.cond(
        votingBase.dateStart.isAfter(now) && votingBase.status == VotingStatus.Active,
        (),
        VotingError.VotingAlreadyStarted)
    } yield votingBase
  }

  def validateVotingCompleted(state: ContractKeysResponse): Either[VotingError, VotingBase] = {
    for {
      votingBase <- state.getVotingBase
      _          <- Either.cond(votingBase.status == VotingStatus.Completed, (), VotingError.VotingIsNotYetFinished)
    } yield votingBase
  }
}
