package com.wavesplatform.voting.contract.validators

import com.wavesplatform.voting.contract.{ServerTrait, ServerType, Status, VotingError}

object ServerListValidator {

  def containsSenderPubKey(servers: Seq[ServerTrait], senderPubKey: String): Either[VotingError, Unit] = {
    Either.cond(
      servers.exists(_.pubKey == senderPubKey),
      (),
      VotingError.ServersDoNotContainSenderPubKey(senderPubKey)
    )
  }

  def validate(servers: Seq[ServerTrait], k: Long): Either[VotingError, Unit] = {
    for {
      _ <- validateServerTypesCount(servers, k)
      _ <- validateServersIndexes(servers)
    } yield ()
  }

  /**
    * Проверить что массив servers[] транзакции не пустой,
    * кол-во серверов в нем больше или равно k, кол-во серверов типа main равно 1
   **/
  private def validateServerTypesCount(servers: Seq[ServerTrait], k: Long): Either[VotingError, Unit] = {
    val activeServersCount = servers.count(_.status == Status.Active)
    val mainServersCount   = servers.count(_.`type` == ServerType.Main)
    Either.cond(activeServersCount >= k && mainServersCount == 1, (), VotingError.InvalidServers(k))
  }

  /**
    * Проверить что каждому серверу из массива servers[] присвоен уникальный порядковый номер i, счет от 1 до n.
    */
  private def validateServersIndexes(servers: Seq[ServerTrait]): Either[VotingError, Unit] = {
    val sortedIndexes    = servers.map(_.i).sorted
    val indexesWithIndex = sortedIndexes.zipWithIndex //zipWithIndex starts from zero that's why I add `1` below
    val indexesIsValid   = indexesWithIndex.forall { case (k, v) => k == v + 1 }
    Either.cond(indexesIsValid, (), VotingError.ServerIndexesAreNotSequential(sortedIndexes))
  }
}
