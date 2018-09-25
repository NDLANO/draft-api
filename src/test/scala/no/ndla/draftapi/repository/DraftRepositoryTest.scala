/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import no.ndla.draftapi.model.domain.{Article, ArticleIds}
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

  test("that importIdOfArticle works correctly") {
    val externalIds = List("1", "2", "3")
    val uuid = "d4e84cd3-ab94-46d5-9839-47ec682d27c2"
    val id1 = 1
    val id2 = 2
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(id1)), externalIds, List.empty, Some(uuid))
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(id2)), List.empty, List.empty, Some(uuid))

    val result1 = repository.importIdOfArticle("1")
    result1.get should be(ArticleIds(id1, externalIds, Some(uuid)))
    val result2 = repository.importIdOfArticle("2")
    result2.get should be(ArticleIds(id1, externalIds, Some(uuid)))

    repository.delete(id1)
    repository.delete(id2)
  }

}
