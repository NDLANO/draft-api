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

  val legacyAuthHeaderWithWriteRole = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOlsiZHJhZnRzOndyaXRlIl0sIm5kbGFfaWQiOiJhYmMxMjMifSwibmFtZSI6IkRvbmFsZCBEdWNrIiwiaXNzIjoiaHR0cHM6Ly9zb21lLWRvbWFpbi8iLCJzdWIiOiJnb29nbGUtb2F1dGgyfDEyMyIsImF1ZCI6ImFiYyIsImV4cCI6MTQ4NjA3MDA2MywiaWF0IjoxNDg2MDM0MDYzfQ.eL4ee3tTIxEObXzd5-TXXmADfsyUao9czrq74cRWPhE"
  val legacyAuthHeaderWithoutAnyRoles = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOltdLCJuZGxhX2lkIjoiYWJjMTIzIn0sIm5hbWUiOiJEb25hbGQgRHVjayIsImlzcyI6Imh0dHBzOi8vc29tZS1kb21haW4vIiwic3ViIjoiZ29vZ2xlLW9hdXRoMnwxMjMiLCJhdWQiOiJhYmMiLCJleHAiOjE0ODYwNzAwNjMsImlhdCI6MTQ4NjAzNDA2M30.kXjaQ9QudcRHTqhfrzKr0Zr4pYISBfJoXWHVBreDyO8"
  val legacyAuthHeaderWithWrongRole = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOlsic29tZTpvdGhlciJdLCJuZGxhX2lkIjoiYWJjMTIzIn0sIm5hbWUiOiJEb25hbGQgRHVjayIsImlzcyI6Imh0dHBzOi8vc29tZS1kb21haW4vIiwic3ViIjoiZ29vZ2xlLW9hdXRoMnwxMjMiLCJhdWQiOiJhYmMiLCJleHAiOjE0ODYwNzAwNjMsImlhdCI6MTQ4NjAzNDA2M30.JsxMW8y0hCmpuu9tpQr6ZdfcqkOS8hRatFi3cTO_PvY"
  val legacyAuthHeaderWithAllRoles = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOlsiYXJ0aWNsZXM6cHVibGlzaCIsImRyYWZ0czp3cml0ZSIsImRyYWZ0czpzZXRfdG9fcHVibGlzaCJdLCJuZGxhX2lkIjoiYWJjMTIzIn0sIm5hbWUiOiJEb25hbGQgRHVjayIsImlzcyI6Imh0dHBzOi8vc29tZS1kb21haW4vIiwic3ViIjoiZ29vZ2xlLW9hdXRoMnwxMjMiLCJhdWQiOiJhYmMiLCJleHAiOjE0ODYwNzAwNjMsImlhdCI6MTQ4NjAzNDA2M30.PXApIH0rT2YlGDNZfvNdSDJyDu6_HC5sQ99UXM1TBdc"

  val authHeaderWithWriteRole = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoiZHJhZnRzOndyaXRlIiwiZ3R5IjoiY2xpZW50LWNyZWRlbnRpYWxzIn0.i_wvbN24VZMqOTQPiEqvqKZy23-m-2ZxTligof8n33k3z-BjXqn4bhKTv7sFdQG9Wf9TFx8UzjoOQ6efQgpbRzl8blZ-6jAZOy6xDjDW0dIwE0zWD8riG8l27iQ88fbY_uCyIODyYp2JNbVmWZNJ9crKKevKmhcXvMRUTrcyE9g"
  val authHeaderWithoutAnyRoles = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoiIiwiZ3R5IjoiY2xpZW50LWNyZWRlbnRpYWxzIn0.fb9eTuBwIlbGDgDKBQ5FVpuSUdgDVBZjCenkOrWLzUByVCcaFhbFU8CVTWWKhKJqt6u-09-99hh86szURLqwl3F5rxSX9PrnbyhI9LsPut_3fr6vezs6592jPJRbdBz3-xLN0XY5HIiJElJD3Wb52obTqJCrMAKLZ5x_GLKGhcY"
  val authHeaderWithWrongRole = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoic29tZTpvdGhlciIsImd0eSI6ImNsaWVudC1jcmVkZW50aWFscyJ9.Hbmh9KX19nx7yT3rEcP9pyzRO0uQJBRucfqH9QEZtLyXjYj_fAyOhsoicOVEbHSES7rtdiJK43-gijSpWWmGWOkE6Ym7nHGhB_nLdvp_25PDgdKHo-KawZdAyIcJFr5_t3CJ2Z2IPVbrXwUd99vuXEBaV0dMwkT0kDtkwHuS-8E"
  val authHeaderWithAllRoles = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoiYXJ0aWNsZXM6cHVibGlzaCBkcmFmdHM6d3JpdGUgZHJhZnRzOnNldF90b19wdWJsaXNoIiwiZ3R5IjoiY2xpZW50LWNyZWRlbnRpYWxzIn0.gsM-U84ykgaxMSbL55w6UYIIQUouPIB6YOmJuj1KhLFnrYctu5vwYBo80zyr1je9kO_6L-rI7SUnrHVao9DFBZJmfFfeojTxIT3CE58hoCdxZQZdPUGePjQzROWRWeDfG96iqhRcepjbVF9pMhKp6FNqEVOxkX00RZg9vFT8iMM"

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

    get("/test/", "ids" -> "1,2,3,4", "page-size" -> "10", "language" -> "nb") {
      status should equal (200)
      verify(articleSearchService, times(1)).all(List(1, 2, 3, 4), Language.DefaultLanguage, None, 1, 4, Sort.ByTitleAsc, ArticleType.all)
    }
  }

  test("POST / should return 400 if body does not contain all required fields") {
    post("/test/", invalidArticle, headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal(400)
    }
  }

  test("POST / should return 201 on created") {
    when(writeService.newArticle(any[NewArticle], any[Option[String]], any[Seq[String]])).thenReturn(Success(TestData.sampleArticleV2))
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
    when(writeService.updateArticle(any[Long], any[api.UpdatedArticle], any[Option[String]], any[Seq[String]])).thenReturn(Failure(new AccessDeniedException("Not today")))

    patch("/test/123", body=write(TestData.sampleApiUpdateArticle)) {
      status should equal (403)
    }
  }

  test("That PATCH /:id returns 200 on success") {
    when(writeService.updateArticle(any[Long], any[UpdatedArticle], any[Option[String]], any[Seq[String]])).thenReturn(Success(TestData.apiArticleWithHtmlFaultV2))
    patch("/test/123", updateTitleJson, headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal (200)
    }
  }

  test("PUT /:id/publish should return 403 if user does not have the required role") {
    when(writeService.queueArticleForPublish(any[Long])).thenReturn(Failure(new AccessDeniedException("Not today")))
    put("/test/1/publish") {
      status should equal (403)
    }
  }

  test("PUT /:id/publish should return 204 if user has required permissions") {
    when(writeService.queueArticleForPublish(any[Long])).thenReturn(Success(api.ArticleStatus(Set.empty)))
    put("/test/1/publish", headers=Map("Authorization" -> authHeaderWithAllRoles)) {
      status should equal (200)
    }
  }

  test("PUT /:id/validate should return 204 if user has required permissions") {
    when(contentValidator.validateArticleApiArticle(any[Long])).thenReturn(Success(TestData.sampleDomainArticle))
    put("/test/1/validate", headers=Map("Authorization" -> authHeaderWithAllRoles)) {
      status should equal (204)
    }
  }

  // Legacy tests. May be removed when the legacy token format in ndla.network v0.24 is removed
  test("LEGACY - POST / should return 400 if body does not contain all required fields") {
    post("/test/", invalidArticle, headers = Map("Authorization" -> legacyAuthHeaderWithWriteRole)) {
      status should equal(400)
    }
  }

  test("LEGACY - POST / should return 201 on created") {
    when(writeService.newArticle(any[NewArticle], any[Option[String]], any[Seq[String]])).thenReturn(Success(TestData.sampleArticleV2))
    post("/test/", write(TestData.newArticle), headers = Map("Authorization" -> legacyAuthHeaderWithWriteRole)) {
      status should equal(201)
    }
  }

  test("LEGACY - That / returns a validation message if article is invalid") {
    post("/test", headers = Map("Authorization" -> legacyAuthHeaderWithWriteRole)) {
      status should equal (400)
    }
  }

  test("LEGACY - That POST / returns 403 if no auth-header") {
    post("/test") {
      status should equal (403)
    }
  }

  test("LEGACY - That POST / returns 403 if auth header does not have expected role") {
    post("/test", headers = Map("Authorization" -> legacyAuthHeaderWithWrongRole)) {
      status should equal (403)
    }
  }

  test("LEGACY - That POST / returns 403 if auth header does not have any roles") {
    post("/test", headers = Map("Authorization" -> legacyAuthHeaderWithoutAnyRoles)) {
      status should equal (403)
    }
  }

  test("LEGACY - That PATCH /:id returns a validation message if article is invalid") {
    patch("/test/123", invalidArticle, headers = Map("Authorization" -> legacyAuthHeaderWithWriteRole)) {
      status should equal (400)
    }
  }

  test("LEGACY - That PATCH /:id returns 403 if no auth-header") {
    patch("/test/123") {
      status should equal (403)
    }
  }

  test("LEGACY - That PATCH /:id returns 403 if auth header does not have expected role") {
    patch("/test/123", headers = Map("Authorization" -> legacyAuthHeaderWithWrongRole)) {
      status should equal (403)
    }
  }

  test("LEGACY - That PATCH /:id returns 403 if auth header does not have any roles") {
    patch("/test/123", headers = Map("Authorization" -> legacyAuthHeaderWithoutAnyRoles)) {
      status should equal (403)
    }
  }

  test("LEGACY - That PATCH /:id returns 200 on success") {
    when(writeService.updateArticle(any[Long], any[UpdatedArticle], any[Option[String]], any[Seq[String]])).thenReturn(Success(TestData.apiArticleWithHtmlFaultV2))
    patch("/test/123", updateTitleJson, headers = Map("Authorization" -> legacyAuthHeaderWithWriteRole)) {
      status should equal (200)
    }
  }

  test("LEGACY - PUT /:id/publish should return 403 if user does not have the required role") {
    when(writeService.queueArticleForPublish(any[Long])).thenReturn(Failure(new AccessDeniedException("Not today")))
    put("/test/1/publish") {
      status should equal (403)
    }
  }

  test("LEGACY - PUT /:id/publish should return 204 if user has required permissions") {
    when(writeService.queueArticleForPublish(any[Long])).thenReturn(Success(api.ArticleStatus(Set.empty)))
    put("/test/1/publish", headers=Map("Authorization" -> legacyAuthHeaderWithAllRoles)) {
      status should equal (200)
    }
  }

}
