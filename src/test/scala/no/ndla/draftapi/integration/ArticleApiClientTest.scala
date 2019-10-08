/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import java.util.Date

import no.ndla.draftapi.model.api.ContentId
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.{IntegrationSuite, TestEnvironment}
import no.ndla.network.AuthUser
import org.json4s.native.Serialization.write
import org.json4s.{DefaultFormats, Formats}

import scala.util.Success

class ArticleApiClientTest extends IntegrationSuite with TestEnvironment {
  implicit val formats: DefaultFormats = DefaultFormats
  override val ndlaClient = new NdlaClient

  // Pact CDC imports
  import com.itv.scalapact.ScalaPactForger._
  import com.itv.scalapact.argonaut62._
  import com.itv.scalapact.http4s16a._

  val idResponse = ContentId(1)
  override val converterService = new ConverterService

  val testCopyright = domain.Copyright(
    Some("CC-BY-SA-4.0"),
    Some("Origin"),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    None,
    None,
    None
  )

  val testArticle = domain.Article(
    id = Some(1),
    revision = Some(1),
    status = domain.Status(domain.ArticleStatus.PUBLISHED, Set.empty),
    title = Seq(domain.ArticleTitle("Title", "nb")),
    content = Seq(domain.ArticleContent("Content", "nb")),
    copyright = Some(testCopyright),
    tags = Seq(domain.ArticleTag(List("Tag1", "Tag2", "Tag3"), "nb")),
    requiredLibraries = Seq(),
    visualElement = Seq(),
    introduction = Seq(),
    metaDescription = Seq(domain.ArticleMetaDescription("Meta Description", "nb")),
    metaImage = Seq(),
    created = new Date(0),
    updated = new Date(0),
    updatedBy = "updatedBy",
    published = new Date(0),
    articleType = domain.ArticleType.Standard,
    notes = Seq.empty,
    previousVersionsNotes = Seq.empty
  )

  val exampleToken =
    "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjogInh4eHl5eSIsICJpc3MiOiAiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCAic3ViIjogInh4eHl5eUBjbGllbnRzIiwgImF1ZCI6ICJuZGxhX3N5c3RlbSIsICJpYXQiOiAxNTEwMzA1NzczLCAiZXhwIjogMTUxMDM5MjE3MywgInNjb3BlIjogImFydGljbGVzLXRlc3Q6cHVibGlzaCBkcmFmdHMtdGVzdDp3cml0ZSBkcmFmdHMtdGVzdDpzZXRfdG9fcHVibGlzaCBhcnRpY2xlcy10ZXN0OndyaXRlIiwgImd0eSI6ICJjbGllbnQtY3JlZGVudGlhbHMifQ.gsM-U84ykgaxMSbL55w6UYIIQUouPIB6YOmJuj1KhLFnrYctu5vwYBo80zyr1je9kO_6L-rI7SUnrHVao9DFBZJmfFfeojTxIT3CE58hoCdxZQZdPUGePjQzROWRWeDfG96iqhRcepjbVF9pMhKp6FNqEVOxkX00RZg9vFT8iMM"
  val authHeaderMap = Map("Authorization" -> s"Bearer $exampleToken")

  test("that allocating an article and concept id should return a long") {
    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Allocating an concept id should return a long")
          .given("empty")
          .uponReceiving(method = POST,
                         path = "/intern/id/concept/allocate",
                         query = None,
                         headers = authHeaderMap,
                         body = None,
                         matchingRules = None)
          .willRespondWith(
            status = 200,
            headers = Map.empty,
            body = write(idResponse),
            matchingRules = None
          )
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(s"Bearer $exampleToken")
        val articleApiClient = new ArticleApiClient(mockConfig.baseUrl)
        articleApiClient.allocateConceptId(List("1234", "4567")) should be(Success(1))
      }
  }

  test("that updating articles and concepts should work") {
    implicit val formats: Formats = domain.Article.formats
    val draftConcept = domain.Concept(
      id = Some(1),
      title = Seq(domain.ConceptTitle("Title", "nb")),
      content = Seq(domain.ConceptContent("Title", "nb")),
      copyright = Some(testCopyright),
      created = new Date(0),
      updated = new Date(0)
    )
    val conceptToUpdate = converterService.toArticleApiConcept(draftConcept)

    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Updating an article returns 200")
          .given("articles")
          .uponReceiving(method = POST,
                         path = "/intern/article/1",
                         query = None,
                         headers = authHeaderMap,
                         body = write(testArticle),
                         matchingRules = None)
          .willRespondWith(200)
      )
      .addInteraction(
        interaction
          .description("Updating a concept returns 200")
          .given("concepts")
          .uponReceiving(method = POST,
                         path = "/intern/concept/1",
                         query = None,
                         headers = authHeaderMap,
                         body = write(conceptToUpdate),
                         matchingRules = None)
          .willRespondWith(200)
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(s"Bearer $exampleToken")
        val articleApiClient = new ArticleApiClient(mockConfig.baseUrl)
        articleApiClient.updateArticle(1, testArticle, List("1234"))
        articleApiClient.updateConcept(1, conceptToUpdate)
      }
  }

  test("that deleting an article or a concept should return 200") {
    val contentId = ContentId(1)

    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Deleting an article should return 200")
          .given("articles")
          .uponReceiving(method = DELETE,
                         path = "/intern/article/1/",
                         query = None,
                         headers = authHeaderMap,
                         body = None,
                         matchingRules = None)
          .willRespondWith(200, write(contentId))
      )
      .addInteraction(
        interaction
          .description("Deleting a concept should return 200")
          .given("concepts")
          .uponReceiving(method = DELETE,
                         path = "/intern/concept/1/",
                         query = None,
                         headers = authHeaderMap,
                         body = None,
                         matchingRules = None)
          .willRespondWith(200, write(contentId))
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(s"Bearer $exampleToken")
        val articleApiClient = new ArticleApiClient(mockConfig.baseUrl)
        articleApiClient.deleteArticle(1) should be(Success(contentId))
        articleApiClient.deleteConcept(1) should be(Success(contentId))
      }
  }

  test("that unpublishing an article returns 200") {
    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Unpublishing an article should return 200")
          .given("articles")
          .uponReceiving(method = POST,
                         path = "/intern/article/1/unpublish/",
                         query = None,
                         headers = authHeaderMap,
                         body = None,
                         matchingRules = None)
          .willRespondWith(200, write(ContentId(1)))
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(s"Bearer $exampleToken")
        val articleApiCient = new ArticleApiClient(mockConfig.baseUrl)
        articleApiCient.unpublishArticle(testArticle).isSuccess should be(true)
      }
  }

  test("that verifying an article returns 200 if valid") {
    val articleApiArticle = converterService.toArticleApiArticle(testArticle)
    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Validating article returns 200")
          .given("empty")
          .uponReceiving(method = POST,
                         path = "/intern/validate/article",
                         query = None,
                         headers = authHeaderMap,
                         body = write(articleApiArticle),
                         matchingRules = None)
          .willRespondWith(200, write(articleApiArticle))
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(s"Bearer $exampleToken")
        val articleApiCient = new ArticleApiClient(mockConfig.baseUrl)
        articleApiCient.validateArticle(articleApiArticle, importValidate = false).isSuccess should be(true)
      }
  }
}
