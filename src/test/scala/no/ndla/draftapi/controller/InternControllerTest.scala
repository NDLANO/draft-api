/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.model.api.ContentId
import no.ndla.draftapi.{DraftSwagger, TestEnvironment, UnitSuite}
import no.ndla.draftapi.TestData.authHeaderWithWriteRole
import no.ndla.draftapi.model.domain.{ArticleIds, ImportId}
import no.ndla.draftapi.{DraftSwagger, TestData, TestEnvironment, UnitSuite}
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatra.test.scalatest.ScalatraFunSuite

import scala.util.{Failure, Success}

class InternControllerTest extends UnitSuite with TestEnvironment with ScalatraFunSuite {
  implicit val formats = org.json4s.DefaultFormats
  implicit val swagger = new DraftSwagger

  lazy val controller = new InternController
  addServlet(controller, "/test")

  test("that deleting an article goes several attempts if call to article-api fails") {
    val failedApiCall = Failure(new RuntimeException("Api call failed :/"))

    when(articleApiClient.deleteArticle(any[Long])).thenReturn(
      failedApiCall,
      failedApiCall,
      failedApiCall,
      Success(ContentId(10))
    )

    when(user.getUser).thenReturn(TestData.userWithWriteAccess)
    delete(s"/test/article/10/") {
      verify(articleApiClient, times(4)).deleteArticle(10)
    }
  }

  test("that getting ids returns 404 for missing and 200 for existing") {
    val uuid = "16d4668f-0917-488b-9b4a-8f7be33bb72a"

    when(readService.importIdOfArticle("1234")).thenReturn(None)
    get(s"/test/import-id/1234") { status should be(404) }

    when(readService.importIdOfArticle("1234")).thenReturn(Some(ImportId(Some(uuid))))
    get("/test/import-id/1234") {
      status should be(200)
      body should be(s"""{"importId":"$uuid"}""".stripMargin)
    }
  }
}
