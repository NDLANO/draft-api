/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import no.ndla.draftapi.{DBMigrator, IntegrationSuite, TestData, TestEnvironment}
import no.ndla.tag.IntegrationTest
import scalikejdbc.{ConnectionPool, DataSourceConnectionPool}

@IntegrationTest
class DraftRepositoryTest extends IntegrationSuite with TestEnvironment {
  var repository: ArticleRepository = _

  val sampleArticle = TestData.sampleArticleWithByNcSa

  override def beforeEach() = {
    repository = new ArticleRepository()
  }

  override def beforeAll() = {
    ConnectionPool.singleton(new DataSourceConnectionPool(getDataSource))
    DBMigrator.migrate(getDataSource)
  }

}
