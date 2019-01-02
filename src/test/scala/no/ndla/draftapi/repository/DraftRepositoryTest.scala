/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import java.net.Socket

import no.ndla.draftapi.model.domain._
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
    val datasource = testDataSource
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
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(idWithExternals)), externalIds, List.empty, None)
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(idWithoutExternals)), List.empty, List.empty, None)

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

  test("that importIdOfArticle works correctly") {
    assume(databaseIsAvailable, "Database is unavailable")
    val externalIds = List("1", "2", "3")
    val uuid = "d4e84cd3-ab94-46d5-9839-47ec682d27c2"
    val id1 = 1
    val id2 = 2
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(id1)), externalIds, List.empty, Some(uuid))
    repository.insertWithExternalIds(sampleArticle.copy(id = Some(id2)), List.empty, List.empty, Some(uuid))

    val result1 = repository.importIdOfArticle("1")
    result1.get should be(ImportId(Some(uuid)))
    val result2 = repository.importIdOfArticle("2")
    result2.get should be(ImportId(Some(uuid)))

    repository.deleteArticle(id1)
    repository.deleteArticle(id2)
  }

  test("ExternalIds should not contains NULLs") {
    assume(databaseIsAvailable, "Database is unavailable")
    val art1 = sampleArticle.copy(id = Some(10))
    repository.insertWithExternalIds(art1, null, List.empty, None)
    val result1 = repository.getExternalIdsFromId(10)

    result1 should be(List.empty)
  }

  test("Updating an article should work as expected") {
    assume(databaseIsAvailable, "Database is unavailable")
    val art1 = sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art2 = sampleArticle.copy(id = Some(2), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art3 = sampleArticle.copy(id = Some(3), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art4 = sampleArticle.copy(id = Some(4), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))

    repository.insert(art1)
    repository.insert(art2)
    repository.insert(art3)
    repository.insert(art4)

    val updatedContent = Seq(ArticleContent("What u do mr", "nb"))

    repository.updateArticle(art1.copy(content = updatedContent))

    repository.withId(art1.id.get).get.content should be(updatedContent)
    repository.withId(art2.id.get).get.content should be(art2.content)
    repository.withId(art3.id.get).get.content should be(art3.content)
    repository.withId(art4.id.get).get.content should be(art4.content)
  }

  test("That storing an article an retrieving it returns the original article") {
    assume(databaseIsAvailable, "Database is unavailable")
    val art1 = sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art2 = sampleArticle.copy(id = Some(2), status = domain.Status(domain.ArticleStatus.PUBLISHED, Set.empty))
    val art3 = sampleArticle.copy(id = Some(3),
                                  status = domain.Status(domain.ArticleStatus.AWAITING_QUALITY_ASSURANCE, Set.empty))
    val art4 = sampleArticle.copy(id = Some(4), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))

    repository.insert(art1)
    repository.insertWithExternalIds(art2, List("1234", "5678"), List.empty, None)
    repository.insert(art3)
    repository.insert(art4)

    repository.withId(art1.id.get).get should be(art1)
    repository.withId(art2.id.get).get should be(art2)
    repository.withId(art3.id.get).get should be(art3)
    repository.withId(art4.id.get).get should be(art4)
  }

  test("That updateWithExternalIds updates article correctly") {
    assume(databaseIsAvailable, "Database is unavailable")
    val art1 = sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    repository.insertWithExternalIds(art1, List("1234", "5678"), List.empty, None)

    val updatedContent = Seq(ArticleContent("This is updated with external ids yo", "en"))
    val updatedArt = art1.copy(content = updatedContent)
    repository.updateWithExternalIds(updatedArt, List("1234", "5678"), List.empty, None)
    repository.withId(art1.id.get).get should be(updatedArt)
  }

  test("That getAllIds returns all articles") {
    assume(databaseIsAvailable, "Database is unavailable")
    val art1 = sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art2 = sampleArticle.copy(id = Some(2), status = domain.Status(domain.ArticleStatus.PUBLISHED, Set.empty))
    val art3 = sampleArticle.copy(id = Some(3), status = domain.Status(domain.ArticleStatus.USER_TEST, Set.empty))
    val art4 = sampleArticle.copy(id = Some(4), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))

    repository.insert(art1)
    repository.insertWithExternalIds(art2, List("1234", "5678"), List.empty, None)
    repository.insert(art3)
    repository.insert(art4)

    repository.getAllIds should be(
      Seq(
        domain.ArticleIds(art1.id.get, List.empty),
        domain.ArticleIds(art2.id.get, List("1234", "5678")),
        domain.ArticleIds(art3.id.get, List.empty),
        domain.ArticleIds(art4.id.get, List.empty),
      ))
  }

  test("that getIdFromExternalId returns id of article correctly") {
    assume(databaseIsAvailable, "Database is unavailable")
    val art1 = sampleArticle.copy(id = Some(14), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    repository.insert(art1)
    repository.insertWithExternalIds(art1.copy(revision = Some(3)), List("5678"), List.empty, None)

    repository.getIdFromExternalId("5678") should be(art1.id)
    repository.getIdFromExternalId("9999") should be(None)
  }

  test("That newArticleId creates the latest available article_id") {
    assume(databaseIsAvailable, "Database is unavailable")
    repository.insert(sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(sampleArticle.copy(id = Some(2), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(sampleArticle.copy(id = Some(3), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(sampleArticle.copy(id = Some(4), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(sampleArticle.copy(id = Some(5), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))

    repository.newArticleId() should be(Success(6))
  }

  test("That idsWithStatus returns correct drafts") {
    assume(databaseIsAvailable, "Database is unavailable")
    repository.insert(sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(sampleArticle.copy(id = Some(2), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insert(
      sampleArticle.copy(id = Some(3), status = domain.Status(domain.ArticleStatus.PROPOSAL, Set.empty)))
    repository.insert(sampleArticle.copy(id = Some(4), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty)))
    repository.insertWithExternalIds(
      sampleArticle.copy(id = Some(5), status = domain.Status(domain.ArticleStatus.PROPOSAL, Set.empty)),
      List("1234"),
      List.empty,
      None)
    repository.insert(
      sampleArticle.copy(id = Some(6), status = domain.Status(domain.ArticleStatus.PUBLISHED, Set.empty)))
    repository.insert(
      sampleArticle.copy(id = Some(7), status = domain.Status(domain.ArticleStatus.QUEUED_FOR_PUBLISHING, Set.empty)))
    repository.insertWithExternalIds(
      sampleArticle.copy(id = Some(8), status = domain.Status(domain.ArticleStatus.PROPOSAL, Set.empty)),
      List("5678", "1111"),
      List.empty,
      None)

    repository.idsWithStatus(ArticleStatus.DRAFT) should be(
      Success(List(ArticleIds(1, List.empty), ArticleIds(2, List.empty), ArticleIds(4, List.empty))))

    repository.idsWithStatus(ArticleStatus.PROPOSAL) should be(
      Success(List(ArticleIds(3, List.empty), ArticleIds(5, List("1234")), ArticleIds(8, List("5678", "1111")))))

    repository.idsWithStatus(ArticleStatus.PUBLISHED) should be(Success(List(ArticleIds(6, List.empty))))

    repository.idsWithStatus(ArticleStatus.QUEUED_FOR_PUBLISHING) should be(Success(List(ArticleIds(7, List.empty))))
  }

  test("That getArticlesByPage returns all latest articles") {
    assume(databaseIsAvailable, "Database is unavailable")
    val art1 = sampleArticle.copy(id = Some(1), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art2 = sampleArticle.copy(id = Some(1),
                                  revision = Some(2),
                                  status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art3 = sampleArticle.copy(id = Some(2), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art4 = sampleArticle.copy(id = Some(3), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art5 = sampleArticle.copy(id = Some(4), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    val art6 = sampleArticle.copy(id = Some(5), status = domain.Status(domain.ArticleStatus.DRAFT, Set.empty))
    repository.insert(art1)
    repository.insert(art2)
    repository.insert(art3)
    repository.insert(art4)
    repository.insert(art5)
    repository.insert(art6)

    val pageSize = 4
    repository.getArticlesByPage(pageSize, pageSize * 0) should be(
      Seq(
        art2,
        art3,
        art4,
        art5
      ))
    repository.getArticlesByPage(pageSize, pageSize * 1) should be(
      Seq(
        art6
      ))
  }
}
