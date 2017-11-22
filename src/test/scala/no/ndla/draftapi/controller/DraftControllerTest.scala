/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain.{ArticleType, Language, SearchResult, Sort}
import no.ndla.draftapi.{DraftSwagger, TestData, TestEnvironment, UnitSuite}
import org.scalatra.test.scalatest.ScalatraFunSuite
import org.mockito.Mockito._
import org.mockito.Matchers._
import no.ndla.mapping.License.getLicenses
import no.ndla.network.AuthUser
import org.json4s.native.Serialization.{read, write}

import scala.util.{Failure, Success}

class DraftControllerTest extends UnitSuite with TestEnvironment with ScalatraFunSuite {

  val jwtHeader = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9"

  val jwtClaims = "eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOlsiZHJhZnRzOndyaXRlIl0sIm5kbGFfaWQiOiJhYmMxMjMifSwibmFtZSI6IkRvbmFsZCBEdWNrIiwiaXNzIjoiaHR0cHM6Ly9zb21lLWRvbWFpbi8iLCJzdWIiOiJnb29nbGUtb2F1dGgyfDEyMyIsImF1ZCI6ImFiYyIsImV4cCI6MTQ4NjA3MDA2MywiaWF0IjoxNDg2MDM0MDYzfQ"
  val jwtClainsAllRoles = "eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOlsiYXJ0aWNsZXM6cHVibGlzaCIsImRyYWZ0czp3cml0ZSIsImRyYWZ0czpzZXRfdG9fcHVibGlzaCJdLCJuZGxhX2lkIjoiYWJjMTIzIn0sIm5hbWUiOiJEb25hbGQgRHVjayIsImlzcyI6Imh0dHBzOi8vc29tZS1kb21haW4vIiwic3ViIjoiZ29vZ2xlLW9hdXRoMnwxMjMiLCJhdWQiOiJhYmMiLCJleHAiOjE0ODYwNzAwNjMsImlhdCI6MTQ4NjAzNDA2M30"
  val jwtClaimsNoRoles = "eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOltdLCJuZGxhX2lkIjoiYWJjMTIzIn0sIm5hbWUiOiJEb25hbGQgRHVjayIsImlzcyI6Imh0dHBzOi8vc29tZS1kb21haW4vIiwic3ViIjoiZ29vZ2xlLW9hdXRoMnwxMjMiLCJhdWQiOiJhYmMiLCJleHAiOjE0ODYwNzAwNjMsImlhdCI6MTQ4NjAzNDA2M30"
  val jwtClaimsWrongRole = "eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOlsic29tZTpvdGhlciJdLCJuZGxhX2lkIjoiYWJjMTIzIn0sIm5hbWUiOiJEb25hbGQgRHVjayIsImlzcyI6Imh0dHBzOi8vc29tZS1kb21haW4vIiwic3ViIjoiZ29vZ2xlLW9hdXRoMnwxMjMiLCJhdWQiOiJhYmMiLCJleHAiOjE0ODYwNzAwNjMsImlhdCI6MTQ4NjAzNDA2M30"

  val authHeadWithAllRoles = s"Bearer $jwtHeader.$jwtClainsAllRoles.PXApIH0rT2YlGDNZfvNdSDJyDu6_HC5sQ99UXM1TBdc"
  val authHeaderWithWriteRole = s"Bearer $jwtHeader.$jwtClaims.VxqM2bu2UF8IAalibIgdRdmsTDDWKEYpKzHPbCJcFzA"
  val authHeaderWithoutAnyRoles = s"Bearer $jwtHeader.$jwtClaimsNoRoles.kXjaQ9QudcRHTqhfrzKr0Zr4pYISBfJoXWHVBreDyO8"
  val authHeaderWithWrongRole = s"Bearer $jwtHeader.$jwtClaimsWrongRole.JsxMW8y0hCmpuu9tpQr6ZdfcqkOS8hRatFi3cTO_PvY"

  implicit val formats = org.json4s.DefaultFormats
  implicit val swagger = new DraftSwagger

  lazy val controller = new DraftController
  addServlet(controller, "/test")

  val updateTitleJson = """{"revision": 1, "title": "hehe", "language": "nb", "content": "content"}"""
  val invalidArticle = """{"revision": 1, "title": [{"language": "nb", "titlee": "lol"]}""" // typo in "titlee"
  val invalidNewArticle = """{ "language": "nb", "content": "<section><h2>Hi</h2></section>" }""" // missing title
  val lang = "nb"
  val articleId = 1

  test("/<article_id> should return 200 if the cover was found withId") {
    when(readService.withId(articleId, lang)).thenReturn(Some(TestData.sampleArticleV2))

    get(s"/test/$articleId?language=$lang") {
      status should equal(200)
    }
  }

  test("/<article_id> should return 404 if the article was not found withId") {
    when(readService.withId(articleId, lang)).thenReturn(None)

    get(s"/test/$articleId?language=$lang") {
      status should equal(404)
    }
  }

  test("/<article_id> should return 400 if the article was not found withId") {
    get(s"/test/one") {
      status should equal(400)
    }
  }

  test("POST / should return 400 if body does not contain all required fields") {
    post("/test/", invalidArticle, headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal(400)
    }
  }

  test("POST / should return 201 on created") {
    when(writeService.newArticle(any[NewArticle])).thenReturn(Success(TestData.sampleArticleV2))
    post("/test/", write(TestData.newArticle), headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal(201)
    }
  }

  test("That / returns a validation message if article is invalid") {
    post("/test", headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal (400)
    }
  }

  test("That POST / returns 403 if no auth-header") {
    post("/test") {
      status should equal (403)
    }
  }

  test("That POST / returns 403 if auth header does not have expected role") {
    post("/test", headers = Map("Authorization" -> authHeaderWithWrongRole)) {
      status should equal (403)
    }
  }

  test("That POST / returns 403 if auth header does not have any roles") {
    post("/test", headers = Map("Authorization" -> authHeaderWithoutAnyRoles)) {
      status should equal (403)
    }
  }

  test("That PATCH /:id returns a validation message if article is invalid") {
    patch("/test/123", invalidArticle, headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal (400)
    }
  }

  test("That PATCH /:id returns 403 if access denied") {
    when(writeService.updateArticle(any[Long], any[api.UpdatedArticle])).thenReturn(Failure(new AccessDeniedException("Not today")))

    patch("/test/123", body=write(TestData.sampleApiUpdateArticle)) {
      status should equal (403)
    }
  }

  test("That PATCH /:id returns 200 on success") {
    when(writeService.updateArticle(any[Long], any[UpdatedArticle])).thenReturn(Success(TestData.apiArticleWithHtmlFaultV2))
    patch("/test/123", updateTitleJson, headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal (200)
    }
  }

  test ("That GET /licenses with filter sat to by only returns creative common licenses") {
    val creativeCommonlicenses = getLicenses.filter(_.license.startsWith("by")).map(l => License(l.license, Option(l.description), l.url)).toSet

    get("/test/licenses", "filter" -> "by") {
      status should equal (200)
      val convertedBody = read[Set[License]](body)
      convertedBody should equal(creativeCommonlicenses)
    }
  }

  test ("That GET /licenses with filter not specified returns all licenses") {
    val allLicenses = getLicenses.map(l => License(l.license, Option(l.description), l.url)).toSet

    get("/test/licenses") {
      status should equal (200)
      val convertedBody = read[Set[License]](body)
      convertedBody should equal(allLicenses)
    }
  }

  test("GET / should use size of id-list as page-size if defined") {
    val searchMock = mock[api.SearchResult]
    val searchResultMock = mock[io.searchbox.core.SearchResult]
    when(articleSearchService.all(any[List[Long]], any[String], any[Option[String]], any[Int], any[Int], any[Sort.Value], any[Seq[String]]))
      .thenReturn(searchMock)
    when(searchConverterService.getHits(searchResultMock, "nb")).thenReturn(Seq.empty)

    get("/test/", "ids" -> "1,2,3,4", "page-size" -> "10", "language" -> "nb") {
      status should equal (200)
      verify(articleSearchService, times(1)).all(List(1, 2, 3, 4), Language.DefaultLanguage, None, 1, 4, Sort.ByTitleAsc, ArticleType.all)
    }
  }

  test("PUT /:id/publish should return 403 if user does not have the required role") {
    when(writeService.queueArticleForPublish(any[Long])).thenReturn(Failure(new AccessDeniedException("Not today")))
    put("/test/1/publish") {
      status should equal (403)
    }
  }

  test("PUT /:id/publish should return 204 if user has required permissions") {
    when(writeService.queueArticleForPublish(any[Long])).thenReturn(Success(1: Long))
    put("/test/1/publish", headers=Map("Authorization" -> authHeadWithAllRoles)) {
      status should equal (204)
    }
  }

}
