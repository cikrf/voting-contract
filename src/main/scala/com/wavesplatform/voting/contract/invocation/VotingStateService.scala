package com.wavesplatform.voting.contract.invocation

import com.wavesplatform.protobuf.service._
import com.wavesplatform.voting.contract._
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._

import scala.concurrent.{ExecutionContext, Future}

class VotingStateService(client: ContractServiceClient, authToken: String)(implicit ec: ExecutionContext) {

  def requestContractKeys(contractTransaction: ContractTransaction, keys: Seq[String]): Future[ContractKeysResponse] = {
    client
      .getContractKeys()
      .withAuth(authToken)
      .invoke(ContractKeysRequest(contractTransaction.contractId, keysFilter = Some(KeysFilter(keys))))
  }

  def requestWithServers(contractTransaction: ContractTransaction, keys: Seq[String]): Future[ContractKeysResponse] = {
    require(keys.contains("SERVERS"), "No 'SERVERS' key found")
    for {
      response <- requestContractKeys(contractTransaction, keys)
      serversList <- response.getServersList
        .fold(e => Future.failed(new RuntimeException(e.errorMessage)), Future.successful)
      servers <- requestContractKeys(contractTransaction, serversList)
    } yield ContractKeysResponse(response.entries ++ servers.entries)
  }
}
