package com.wavesplatform.voting.contract

import java.math.BigInteger
import java.time.Instant

sealed trait VotingError {
  def errorMessage: String
}

object VotingError {

  final case class GenericVotingError(errorMessage: String) extends VotingError

  final case class RequiredParamIsMissing(param: String) extends VotingError {
    override def errorMessage: String = s"Parameter '$param' is missing"
  }

  final case class RequiredParamValueMissing(param: String) extends VotingError {
    override def errorMessage: String = s"Parameter '$param' is present, but value is None"
  }

  final case class StartDateError(startDate: Instant, endDate: Instant) extends VotingError {
    override def errorMessage: String = s"Start date '$startDate' is not before end date '$endDate'"
  }

  final case class InvalidServers(k: Long) extends VotingError {
    override def errorMessage: String =
      s"Number of servers must be greater than or equal to k = '$k' and include one server of '${ServerType.Main}' type"
  }

  final case class WrongCallOperationError(op: String) extends VotingError {
    override def errorMessage: String = s"Wrong call contract operation '$op'"
  }

  final object EmptyTransactionField extends VotingError {
    override def errorMessage: String = "Empty transaction field"
  }

  final case class NoResponse(reason: String) extends VotingError {
    override def errorMessage: String = s"State request failed, reason: '$reason'"
  }

  final case class ServerIndexesAreNotSequential(indexes: Seq[Int]) extends VotingError {
    override def errorMessage: String =
      s"Server indexes are not sequential or don't start from 1: [${indexes.mkString(", ")}]"
  }

  final case class ParseError(message: String) extends VotingError {
    override def errorMessage: String = s"Parsing error: '$message'"
  }

  final object VotingAlreadyStarted extends VotingError {
    override def errorMessage: String = s"Voting has already started"
  }

  final object VotingIsNotInProgress extends VotingError {
    override def errorMessage: String = s"Voting is not in progress (has already finished or not yet started)"
  }

  final object VotingIsNotYetFinished extends VotingError {
    override def errorMessage: String = s"Voting is not yet finished (is in progress or not yet started)"
  }

  final case class ServersDoNotContainSenderPubKey(senderPubKey: String) extends VotingError {
    override def errorMessage: String =
      s"Couldn't find an '${Status.Active.entryName}' server of '${ServerType.Main.entryName}' type that contains sender public key '$senderPubKey'"
  }

  final object NoServersWithComplaints extends VotingError {
    override def errorMessage: String = "There are no servers with complaints"
  }

  final case class SenderCantVote(senderPubKey: String) extends VotingError {
    override def errorMessage: String = s"Transaction sender '$senderPubKey' does not have rights to vote"
  }

  final case class ServerAlreadyVoted(round: Long, voterServer: ServerTrait) extends VotingError {
    override def errorMessage: String =
      s"Server has already voted. Round from transaction '$round', voter server round '${voterServer.dkgRound}'"
  }

  final case class WrongRound(round: Long, expectedRound: Long) extends VotingError {
    override def errorMessage: String =
      s"Server has voted in the wrong round. Round from transaction '$round', expected round '$expectedRound'"
  }

  final case class DkgScalarIsNotEmpty(voterServer: ServerTrait) extends VotingError {
    override def errorMessage: String =
      s"DkgScalar is not empty for the server with public key: '${voterServer.pubKey}'"
  }

  final object NotAllServersHavePublishedCommits extends VotingError {
    override def errorMessage: String = "Not all servers have published dkgCommits"
  }

  final case class DkgShadowsNonEmpty(voterServer: ServerTrait) extends VotingError {
    override def errorMessage: String =
      s"DkgShadows is not empty for the server with public key '${voterServer.pubKey}'"
  }

  final object NotAllServersHavePublishedScalar extends VotingError {
    override def errorMessage: String = "Not all server have published dkgScalar"
  }

  final case class ComplaintTransactionDoesNotExist(transactionId: String) extends VotingError {
    override def errorMessage: String = s"Complaint transaction '$transactionId' does not exist"
  }

  final object ServersNotOnTheSameRound extends VotingError {
    override def errorMessage: String = "Servers are not on the same round"
  }

  final object SomeServersHaveComplaints extends VotingError {
    override def errorMessage: String = "Some servers have complaints"
  }

  final case class QuestionDimensionViolation(found: Int, expected: Int) extends VotingError {
    override def errorMessage: String =
      s"Dimension violation: answered questions count '$found' is not equal to expected count '$expected'"
  }

  final case class QuestionOptionsDimensionViolation(found: Int, expected: Int) extends VotingError {
    override def errorMessage: String =
      s"Dimension violation: selected options count '$found' is not equal to expected count '$expected' for question"
  }

  final case class UpdateRoundError(newValue: Long, oldValue: Long) extends VotingError {
    override def errorMessage: String =
      s"Specified round value '$newValue' is less than old value '$oldValue' in VOTING_BASE.dkgRound"
  }

  final case class UnknownEnumParamValue(param: String, value: String) extends VotingError {
    override def errorMessage: String = s"Unknown '$param' parameter value '$value'"
  }

  final case class BlindSigIsNotEqual(oldValue: BigInteger, newValue: BigInteger) extends VotingError {
    override def errorMessage: String =
      s"Existing 'blindSig' value '${oldValue.toString(16)}' is not equal to new value '${newValue.toString(16)}'"
  }

  final case class InvalidBlindSig(blindSig: BigInteger) extends VotingError {
    override def errorMessage: String = s"Verification failed for blind signature '${blindSig.toString(16)}'"
  }
}
