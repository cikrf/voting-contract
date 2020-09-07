package com.wavesplatform.voting.contract.util

import com.wavesplatform.protobuf.service.ContractKeysResponse
import com.wavesplatform.voting.contract.util.DataEntriesExtension._
import com.wavesplatform.voting.contract.{parseJson, ServerTrait, VoteWrapper, VotingBase, VotingError}

import scala.collection.Seq

object ContractKeysResponseExtension {

  implicit class ContractKeysResponseParser(val value: ContractKeysResponse) extends AnyVal {

    def getVotingBase: Either[VotingError, VotingBase] = {
      for {
        votingBaseJsonStr <- value.entries.extractStringParam("VOTING_BASE")
        votingBase        <- parseJson[VotingBase](votingBaseJsonStr)
      } yield votingBase
    }

    def getServersList: Either[VotingError, Seq[String]] = {
      for {
        serversListStr <- value.entries.extractStringParam("SERVERS")
        serversList    <- parseJson[Seq[String]](serversListStr)
      } yield serversList
    }

    //looks ugly
    def getServers: Either[VotingError, Seq[ServerTrait]] = {
      val result: Seq[ServerTrait] = value.entries.foldLeft(Seq.empty[ServerTrait]) {
        case (accum, entry) =>
          if (entry.key.startsWith("SERVER_")) {
            val parseResult = parseJson[ServerTrait](entry.getStringValue)
            if (parseResult.isLeft) {
              return Left(parseResult.left.get)
            } else {
              accum :+ parseResult.right.get
            }
          } else {
            accum
          }
      }

      Right(result)
    }

    def getServer(publicKey: String): Either[VotingError, ServerTrait] = {
      for {
        serverJsonStr <- value.entries.extractStringParam(s"SERVER_$publicKey")
        server        <- parseJson[ServerTrait](serverJsonStr)
      } yield server
    }

    def getVote(publicKey: String): Either[VotingError, Option[VoteWrapper]] = {
      val voteWrapperOpt = value.entries.extractOptionalStringParam(s"VOTE_$publicKey")
      voteWrapperOpt match {
        case Some(value) => parseJson[VoteWrapper](value).map(Some(_))
        case None        => Right(None)
      }
    }
  }
}
