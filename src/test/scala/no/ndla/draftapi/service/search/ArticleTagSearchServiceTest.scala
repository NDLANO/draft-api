/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import java.util.Date

import no.ndla.draftapi.DraftApiProperties.DefaultPageSize
import no.ndla.draftapi._
import no.ndla.draftapi.integration.Elastic4sClientFactory
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.domain
import org.joda.time.DateTime
import org.scalatest.Outcome

import scala.util.Success

class ArticleTagSearchServiceTest extends IntegrationSuite(withSearch = true) with TestEnvironment {

  e4sClient = Elastic4sClientFactory.getClient(elasticSearchHost.getOrElse("http://localhost:9200"))

  // Skip tests if no docker environment available
  override def withFixture(test: NoArgTest): Outcome = {
    assume(elasticSearchContainer.isSuccess)
    super.withFixture(test)
  }

  override val articleTagSearchService = new ArticleTagSearchService
  override val articleTagIndexService = new ArticleTagIndexService
  override val converterService = new ConverterService
  override val searchConverterService = new SearchConverterService

  val article1 = TestData.sampleDomainArticle.copy(
    tags = Seq(
      domain.ArticleTag(
        Seq("test", "testing", "testemer"),
        "nb"
      )
    )
  )

  val article2 = TestData.sampleDomainArticle.copy(
    tags = Seq(
      domain.ArticleTag(
        Seq("test"),
        "en"
      )
    )
  )

  val article3 = TestData.sampleDomainArticle.copy(
    tags = Seq(
      domain.ArticleTag(
        Seq("hei", "test", "testing"),
        "nb"
      ),
      domain.ArticleTag(
        Seq("test"),
        "en"
      )
    )
  )

  val article4 = TestData.sampleDomainArticle.copy(
    tags = Seq(
      domain.ArticleTag(
        Seq("kyllingfilet", "filetkylling"),
        "nb"
      )
    )
  )

  val articlesToIndex = Seq(article1, article2, article3, article4)

  override def beforeAll: Unit = if (elasticSearchContainer.isSuccess) {
    articleTagIndexService.createIndexWithName(DraftApiProperties.DraftTagSearchIndex)

    articlesToIndex.foreach(a => articleTagIndexService.indexDocument(a))

    val allTagsToIndex = articlesToIndex.flatMap(_.tags)
    val groupedByLanguage = allTagsToIndex.groupBy(_.language)
    val tagsDistinctByLanguage = groupedByLanguage.values.flatMap(x => x.flatMap(_.tags).toSet)

    blockUntil(() => articleTagSearchService.countDocuments == tagsDistinctByLanguage.size)
  }

  override def afterAll(): Unit = if (elasticSearchContainer.isSuccess) {
    articleTagIndexService.deleteIndexWithName(Some(DraftApiProperties.DraftTagSearchIndex))
  }

  def blockUntil(predicate: () => Boolean): Unit = {
    var backoff = 0
    var done = false

    while (backoff <= 16 && !done) {
      if (backoff > 0) Thread.sleep(200 * backoff)
      backoff = backoff + 1
      try {
        done = predicate()
      } catch {
        case e: Throwable => println("problem while testing predicate", e)
      }
    }

    require(done, s"Failed waiting for predicate")
  }

  test("That searching for tags returns sensible results") {
    val Success(result) = articleTagSearchService.matchingQuery("test", "nb", 1, 100)

    result.totalCount should be(3)
    result.results should be(Seq("test", "testemer", "testing"))
  }

  test("That only prefixes are matched") {
    val Success(result) = articleTagSearchService.matchingQuery("kylling", "nb", 1, 100)

    result.totalCount should be(1)
    result.results should be(Seq("kyllingfilet"))
  }

}
