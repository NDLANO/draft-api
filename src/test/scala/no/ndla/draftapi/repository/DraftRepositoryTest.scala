/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import no.ndla.draftapi.model.domain.Article
import no.ndla.draftapi.{DBMigrator, IntegrationSuite, TestData, TestEnvironment}
import no.ndla.tag.IntegrationTest
import scalikejdbc.{ConnectionPool, DataSourceConnectionPool}

@IntegrationTest
class DraftRepositoryTest extends IntegrationSuite with TestEnvironment {
  var repository: ArticleRepository = _

  val sampleArticle: Article = TestData.sampleArticleWithByNcSa

  override def beforeEach(): Unit = {
    repository = new ArticleRepository()
  }

  override def beforeAll(): Unit = {
    ConnectionPool.singleton(new DataSourceConnectionPool(getDataSource))
    DBMigrator.migrate(getDataSource)
  }

  test("Fetching external ids works as expected") {
    val externalIds = List("1", "2", "3")
    val idWithExternals = 1
    val idWithoutExternals = 2
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(idWithExternals)), externalIds, List.empty, None)
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(idWithoutExternals)), List.empty, List.empty, None)

    val result1 = repository.getExternalIdsFromId(idWithExternals)
    result1 should be(externalIds)
    val result2 = repository.getExternalIdsFromId(idWithoutExternals)
    result2 should be(List.empty)

    repository.delete(idWithExternals)
    repository.delete(idWithoutExternals)
  }

}
