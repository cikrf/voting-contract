package com.wavesplatform.voting.contract.util

import java.math.BigInteger
import java.time.Instant

import com.wavesplatform.protobuf.service.ContractKeysResponse
import com.wavesplatform.voting.contract.util.ContractKeysResponseExtension._
import com.wavesplatform.voting.contract.util.TestDateTime._
import com.wavesplatform.voting.contract.{PollType, ServerTrait, Status, VotingBase, VotingStatus}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ContractKeysResponseExtensionSpec extends AnyFreeSpec with Matchers {

  private val votingBase: VotingBase = VotingBase(
    pollId = "pollIdValue",
    pollType = PollType.Blind,
    bulletinHash = "BulletinHash",
    dimension = Vector(2, 2, 2),
    status = VotingStatus.Completed,
    dateStart = Instant.now(),
    dateEnd = Some(Instant.now()),
    k = 10,
    dkgRound = 5,
    blindSigModulo = new BigInteger(
      "c8d503b962f15fcc18ae7ae2fba48eb472d8428ad6d496cf27520792a4fd4c7807e8549ef1563d8bf8fdf56e2669e3522ba1860cbc727c888cd2478ae4268929597dc8480ec570c1bb968191451c452e0dd4e651405ef5f0a76164d9715ac4739b7a440cdb7a58d82e5344be8a0ee2acde06c7af061cdb0c75958a755e4e6ddf8da24c1fea3886cb6aa2bfc9b2de8aee5a49250d2aee3a2e016bda701eba249819e78e9e27ac991842abe5075677d6b9eebb1a12d9614eaeaa6fdc2eefc2f0df73ff8955b7b6489c21c2cf8e116a5d2ce50a0cfb6d922d363758e35baee09b8f567a87ead72302d97ea2eb42eb61a619932d40b17e3787637028afb683a0ac99f5bdbfb0ab6f51622755ad04c05528a8aa267b3318e4f51e46c1a05afcd3392d3a7e83098317954207b44768b6da9eb4b39ee9b6b646dd9ba6256fb6a02e7aca2e52fd2a41b2edc2475e2744e417bb8d68e90a6eb3f7416892f185ae54e9251b6cdfb2368ca087d42a6bab9be57ed6dc299d778c76be0b3d2a8e186558a4689f",
      16),
    blindSigExponent = new BigInteger("10001", 16)
  )

  private val server: ServerTrait = ServerTrait.DecryptServer(
    description = "description",
    i = 1,
    pubKey = "pubKey",
    status = Status.Blocked,
    dkgRound = 0,
    dkgScalar = "dkgScalar",
    dkgCommit = "dkgCommit",
    dkgShadows = "[]",
    dkgComplaints = Vector("comp1", "comp2"),
    dkgExponents = "[]"
  )
  private val mainServer: ServerTrait = ServerTrait.MainServer(
    description = "description",
    i = 2,
    pubKey = "pubKey",
    status = Status.Blocked,
    dkgRound = 0,
    dkgScalar = "dkgScalar",
    dkgCommit = "dkgCommit",
    dkgShadows = "[]",
    dkgComplaints = Vector("comp1", "comp2"),
    dkgExponents = "[]"
  )

  private val defaultDataEntries = Seq(
    votingBase.toDataEntry,
    server.toDataEntry,
    mainServer.toDataEntry
  )

  private val contractKeysResponse = ContractKeysResponse(entries = defaultDataEntries)

  "parse VOTING_BASE from ContractKeysResponse.entries" in {
    val parsedVotingBase = contractKeysResponse.getVotingBase
    parsedVotingBase shouldBe 'right
    parsedVotingBase.right.get shouldBe votingBase
      .copy(
        dateStart = votingBase.dateStart.truncateToSeconds(),
        dateEnd = votingBase.dateEnd.map(_.truncateToSeconds()))
  }

  "parse SERVER_{publicKey} from ContractKeysResponse.entries" in {
    val parsedServers = contractKeysResponse.getServers
    parsedServers shouldBe 'right
    parsedServers.right.get should contain theSameElementsAs Seq(server, mainServer)
  }

}
