/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import java.net.Socket

import no.ndla.draftapi.model.domain.Article
import no.ndla.draftapi._
import no.ndla.draftapi.model.domain
import scalikejdbc._

import scala.util.{Success, Try}

class DraftRepositoryTest extends IntegrationSuite with TestEnvironment {
  var repository: ArticleRepository = _

  val sampleArticle: Article = TestData.sampleArticleWithByNcSa

  def emptyTestDatabase = {
    DB autoCommit (implicit session => {
      sql"delete from draftapitest.articledata;".execute.apply()(session)
    })
  }

  def serverIsListening: Boolean = {
    Try(new Socket(DraftApiProperties.MetaServer, DraftApiProperties.MetaPort)) match {
      case Success(c) =>
        c.close()
        true
      case _ => false
    }
  }
  def databaseIsAvailable: Boolean = Try(repository.articleCount).isSuccess

  override def beforeEach(): Unit = {
    repository = new ArticleRepository()
    if (databaseIsAvailable) {
      emptyTestDatabase
    }
  }

  override def beforeAll(): Unit = {
    val datasource = getDataSource
    if (serverIsListening) {
      DBMigrator.migrate(datasource)
      ConnectionPool.singleton(new DataSourceConnectionPool(datasource))
    }
  }

  test("Fetching external ids works as expected") {
    assume(databaseIsAvailable, "Database is unavailable")
    val externalIds = List("1", "2", "3")
    val idWithExternals = 1
    val idWithoutExternals = 2
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(idWithExternals)), externalIds, List.empty)
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(idWithoutExternals)), List.empty, List.empty)

    val result1 = repository.getExternalIdsFromId(idWithExternals)
    result1 should be(externalIds)
    val result2 = repository.getExternalIdsFromId(idWithoutExternals)
    result2 should be(List.empty)
  }

  test("withId only returns non-archieved articles") {
    assume(databaseIsAvailable, "Database is unavailable")
    repository.insert(sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(
      sampleArticle.copy(id = Some(2), status = domain.Status(domain.ArticleStatus.ARCHIVED, Set.empty)))

    repository.withId(1).isDefined should be(true)
    repository.withId(2).isDefined should be(false)
  }

}
