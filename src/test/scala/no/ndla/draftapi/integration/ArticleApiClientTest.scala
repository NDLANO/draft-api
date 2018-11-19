/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import java.util.Date

import no.ndla.draftapi.model.api.{ContentId, Copyright}
import no.ndla.draftapi.model.domain.Status
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.network.AuthUser
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization.write

import scala.util.Success

class ArticleApiClientTest extends UnitSuite with TestEnvironment {
  implicit val formats: DefaultFormats = DefaultFormats
  override val ndlaClient = new NdlaClient

  // Pact CDC imports
  import com.itv.scalapact.ScalaPactForger._
  import com.itv.scalapact.argonaut62._
  import com.itv.scalapact.http4s16a._

  val idResponse = ContentId(1)

  val exampleToken =
    "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjogInh4eHl5eSIsICJpc3MiOiAiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCAic3ViIjogInh4eHl5eUBjbGllbnRzIiwgImF1ZCI6ICJuZGxhX3N5c3RlbSIsICJpYXQiOiAxNTEwMzA1NzczLCAiZXhwIjogMTUxMDM5MjE3MywgInNjb3BlIjogImFydGljbGVzLXRlc3Q6cHVibGlzaCBkcmFmdHMtdGVzdDp3cml0ZSBkcmFmdHMtdGVzdDpzZXRfdG9fcHVibGlzaCBhcnRpY2xlcy10ZXN0OndyaXRlIiwgImd0eSI6ICJjbGllbnQtY3JlZGVudGlhbHMifQ.gsM-U84ykgaxMSbL55w6UYIIQUouPIB6YOmJuj1KhLFnrYctu5vwYBo80zyr1je9kO_6L-rI7SUnrHVao9DFBZJmfFfeojTxIT3CE58hoCdxZQZdPUGePjQzROWRWeDfG96iqhRcepjbVF9pMhKp6FNqEVOxkX00RZg9vFT8iMM"
  val authHeaderMap = Map("Authorization" -> exampleToken)

  test("allocating an article and concept id should return a long") {
    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Allocating an article id should return a long")
          .uponReceiving(method = POST,
                         path = "/intern/id/article/allocate",
                         query = None,
                         headers = authHeaderMap,
                         body = None,
                         matchingRules = None)
          .willRespondWith(
            status = 200,
            headers = Map.empty,
            body = write(idResponse),
            matchingRules = bodyRegexRule("id", "^[0-9]+$") // The id is hard to predict. Just make sure its a number
          )
      )
      .addInteraction(
        interaction
          .description("Allocating an concept id should return a long")
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
            matchingRules = bodyRegexRule("id", "^[0-9]+$") // The id is hard to predict. Just make sure its a number
          )
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(exampleToken)
        val articleApiClient = new ArticleApiClient(mockConfig.baseUrl)
        articleApiClient.allocateArticleId(List("1234", "4567"), List("1", "2")) should be(Success(1))
        articleApiClient.allocateConceptId(List("1234", "4567")) should be(Success(1))
      }
  }

  test("updating articles and concepts should work and return updated version") {
    implicit val formats: Formats = domain.Article.formats
    val copyright = domain.Copyright(
      Some("CC-BY-SA-4.0"),
      Some("Origin"),
      Seq.empty,
      Seq.empty,
      Seq.empty,
      None,
      None,
      None
    )

    val articleToUpdate = domain.Article(
      id = Some(1),
      revision = Some(1),
      status = domain.Status(domain.ArticleStatus.PUBLISHED, Set.empty),
      title = Seq(domain.ArticleTitle("Title", "nb")),
      content = Seq(domain.ArticleContent("Content", "nb")),
      copyright = Some(copyright),
      tags = Seq(domain.ArticleTag(List("Tag1", "Tag2", "Tag3"), "nb")),
      requiredLibraries = Seq(),
      visualElement = Seq(),
      introduction = Seq(),
      metaDescription = Seq(domain.ArticleMetaDescription("Meta Description", "nb")),
      metaImage = Seq(),
      created = new Date(0),
      updated = new Date(0),
      updatedBy = "updatedBy",
      articleType = domain.ArticleType.Standard,
      notes = Seq()
    )

    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Updating an article returns 200")
          .uponReceiving(POST, "/intern/article/1", None, authHeaderMap, write(articleToUpdate), None)
          .willRespondWith(200)
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(exampleToken)
        val articleApiClient = new ArticleApiClient(mockConfig.baseUrl)
        articleApiClient.updateArticle(1, articleToUpdate, List("1234"))
      }
  }
}
