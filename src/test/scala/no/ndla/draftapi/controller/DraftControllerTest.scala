/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import java.util.Date

import no.ndla.draftapi.TestData.authHeaderWithWriteRole
import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.domain.{ArticleType, Language, Sort}
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.{DraftSwagger, TestData, TestEnvironment, UnitSuite}
import no.ndla.mapping.License.getLicenses
import org.json4s.DefaultFormats
import org.json4s.native.Serialization.{read, write}
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatra.test.scalatest.ScalatraFunSuite

import scala.util.{Failure, Success}

class DraftControllerTest extends UnitSuite with TestEnvironment with ScalatraFunSuite {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  implicit val swagger: DraftSwagger = new DraftSwagger

  lazy val controller = new DraftController
  addServlet(controller, "/test")

  val updateTitleJson = """{"revision": 1, "title": "hehe", "language": "nb", "content": "content"}"""
  val invalidArticle = """{"revision": 1, "title": [{"language": "nb", "titlee": "lol"]}""" // typo in "titlee"
  val invalidNewArticle = """{ "language": "nb", "content": "<section><h2>Hi</h2></section>" }""" // missing title
  val lang = "nb"
  val articleId = 1

  override def beforeEach: Unit = {
    when(user.getUser).thenReturn(TestData.userWithWriteAccess)
  }

  test("/<article_id> should return 200 if the cover was found withId") {
    when(readService.withId(articleId, lang)).thenReturn(Success(TestData.sampleArticleV2))

    get(s"/test/$articleId?language=$lang") {
      status should equal(200)
    }
  }

  test("/<article_id> should return 404 if the article was not found withId") {
    when(readService.withId(articleId, lang)).thenReturn(Failure(NotFoundException("not found yo")))

    get(s"/test/$articleId?language=$lang") {
      status should equal(404)
    }
  }

  test("/<article_id> should return 400 if the article was not found withId") {
    get(s"/test/one") {
      status should equal(400)
    }
  }

  test("That GET /licenses/ with filter sat to by only returns creative common licenses") {
    val creativeCommonlicenses =
      getLicenses
        .filter(_.license.toString.startsWith("by"))
        .map(l => License(l.license.toString, Option(l.description), l.url))
        .toSet

    get("/test/licenses/", "filter" -> "by") {
      status should equal(200)
      val convertedBody = read[Set[License]](body)
      convertedBody should equal(creativeCommonlicenses)
    }
  }

  test("That GET /licenses/ with filter not specified returns all licenses") {
    val allLicenses = getLicenses.map(l => License(l.license.toString, Option(l.description), l.url)).toSet

    get("/test/licenses/") {
      status should equal(200)
      val convertedBody = read[Set[License]](body)
      convertedBody should equal(allLicenses)
    }
  }

  test("GET / should use size of id-list as page-size if defined") {
    val searchMock = mock[domain.SearchResult[api.ArticleSummary]]
    when(searchMock.scrollId).thenReturn(None)
    when(
      articleSearchService.all(any[List[Long]],
                               any[String],
                               any[Option[String]],
                               any[Int],
                               any[Int],
                               any[Sort.Value],
                               any[Seq[String]],
                               any[Boolean]))
      .thenReturn(Success(searchMock))

    get("/test/", "ids" -> "1,2,3,4", "page-size" -> "10", "language" -> "nb") {
      status should equal(200)
      verify(articleSearchService, times(1)).all(List(1, 2, 3, 4),
                                                 Language.DefaultLanguage,
                                                 None,
                                                 1,
                                                 4,
                                                 Sort.ByTitleAsc,
                                                 ArticleType.all,
                                                 fallback = false)
    }
  }

  test("POST / should return 400 if body does not contain all required fields") {
    post("/test/", invalidArticle) {
      status should equal(400)
    }
  }

  test("POST / should return 201 on created") {
    when(
      writeService
        .newArticle(any[NewArticle],
                    any[List[String]],
                    any[Seq[String]],
                    any[UserInfo],
                    any[Option[Date]],
                    any[Option[Date]],
                    any[Option[String]]))
      .thenReturn(Success(TestData.sampleArticleV2))
    post("/test/", write(TestData.newArticle), headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal(201)
    }
  }

  test("That / returns a validation message if article is invalid") {
    post("/test") {
      status should equal(400)
    }
  }

  test("That POST / returns 403 if no auth-header") {
    when(user.getUser).thenReturn(TestData.userWithNoRoles)
    post("/test") {
      status should equal(403)
    }
  }

  test("That POST / returns 403 if auth header does not have any roles") {
    when(user.getUser).thenReturn(TestData.userWithNoRoles)

    post("/test") {
      status should equal(403)
    }
  }

  test("That PATCH /:id returns a validation message if article is invalid") {
    patch("/test/123", invalidArticle, headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      status should equal(400)
    }
  }

  test("That PATCH /:id returns 403 if access denied") {
    when(user.getUser).thenReturn(TestData.userWithNoRoles)
    when(
      writeService.updateArticle(any[Long],
                                 any[api.UpdatedArticle],
                                 any[List[String]],
                                 any[Seq[String]],
                                 any[UserInfo],
                                 any[Option[Date]],
                                 any[Option[Date]],
                                 any[Option[String]]))
      .thenReturn(Failure(new AccessDeniedException("Not today")))

    patch("/test/123", body = write(TestData.sampleApiUpdateArticle)) {
      status should equal(403)
    }
  }

  test("That PATCH /:id returns 200 on success") {
    when(
      writeService.updateArticle(any[Long],
                                 any[UpdatedArticle],
                                 any[List[String]],
                                 any[Seq[String]],
                                 any[UserInfo],
                                 any[Option[Date]],
                                 any[Option[Date]],
                                 any[Option[String]]))
      .thenReturn(Success(TestData.apiArticleWithHtmlFaultV2))
    patch("/test/123", updateTitleJson) {
      status should equal(200)
    }
  }

  test("PUT /:id/validate/ should return 204 if user has required permissions") {
    when(contentValidator.validateArticleApiArticle(any[Long], any[Boolean])).thenReturn(Success(ContentId(1)))
    put("/test/1/validate/") {
      status should equal(200)
    }
  }

  test("That scrollId is in header, and not in body") {
    val scrollId =
      "DnF1ZXJ5VGhlbkZldGNoCgAAAAAAAAC1Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAthYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALcWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC4Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuRYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALsWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC9Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuhYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAAL4WLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC8Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFE="
    val searchResponse = domain.SearchResult[api.ArticleSummary](
      0,
      Some(1),
      10,
      "nb",
      Seq.empty[api.ArticleSummary],
      Some(scrollId)
    )
    when(
      articleSearchService.all(any[List[Long]],
                               any[String],
                               any[Option[String]],
                               any[Int],
                               any[Int],
                               any[Sort.Value],
                               any[Seq[String]],
                               any[Boolean]))
      .thenReturn(Success(searchResponse))

    get(s"/test/") {
      status should be(200)
      body.contains(scrollId) should be(false)
      header("search-context") should be(scrollId)
    }
  }

  test("That scrolling uses scroll and not searches normally") {
    reset(articleSearchService)
    val scrollId =
      "DnF1ZXJ5VGhlbkZldGNoCgAAAAAAAAC1Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAthYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALcWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC4Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuRYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALsWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC9Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuhYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAAL4WLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC8Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFE="
    val searchResponse = domain.SearchResult[api.ArticleSummary](
      0,
      Some(1),
      10,
      "nb",
      Seq.empty[api.ArticleSummary],
      Some(scrollId)
    )

    when(articleSearchService.scroll(anyString, anyString)).thenReturn(Success(searchResponse))

    get(s"/test?search-context=$scrollId") {
      status should be(200)
    }

    verify(articleSearchService, times(0)).all(any[List[Long]],
                                               any[String],
                                               any[Option[String]],
                                               any[Int],
                                               any[Int],
                                               any[Sort.Value],
                                               any[Seq[String]],
                                               any[Boolean])
    verify(articleSearchService, times(0)).matchingQuery(any[String],
                                                         any[List[Long]],
                                                         any[String],
                                                         any[Option[String]],
                                                         any[Int],
                                                         any[Int],
                                                         any[Sort.Value],
                                                         any[Seq[String]],
                                                         any[Boolean])
    verify(articleSearchService, times(1)).scroll(eqTo(scrollId), any[String])
  }

  test("That scrolling with POST uses scroll and not searches normally") {
    reset(articleSearchService)
    val scrollId =
      "DnF1ZXJ5VGhlbkZldGNoCgAAAAAAAAC1Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAthYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALcWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC4Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuRYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALsWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC9Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuhYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAAL4WLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC8Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFE="
    val searchResponse = domain.SearchResult[api.ArticleSummary](
      0,
      Some(1),
      10,
      "nb",
      Seq.empty[api.ArticleSummary],
      Some(scrollId)
    )

    when(articleSearchService.scroll(anyString, anyString)).thenReturn(Success(searchResponse))

    post(s"/test/search/?search-context=$scrollId") {
      status should be(200)
    }

    verify(articleSearchService, times(0)).all(any[List[Long]],
                                               any[String],
                                               any[Option[String]],
                                               any[Int],
                                               any[Int],
                                               any[Sort.Value],
                                               any[Seq[String]],
                                               any[Boolean])
    verify(articleSearchService, times(0)).matchingQuery(any[String],
                                                         any[List[Long]],
                                                         any[String],
                                                         any[Option[String]],
                                                         any[Int],
                                                         any[Int],
                                                         any[Sort.Value],
                                                         any[Seq[String]],
                                                         any[Boolean])
    verify(articleSearchService, times(1)).scroll(eqTo(scrollId), any[String])
  }
}
