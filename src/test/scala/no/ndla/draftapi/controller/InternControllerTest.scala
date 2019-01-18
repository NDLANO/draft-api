/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi._
import no.ndla.draftapi.model.api.ContentId
import no.ndla.draftapi.model.domain.ImportId
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

  test("That DELETE /index removes all indexes") {
    reset(articleIndexService)
    reset(conceptIndexService)
    reset(agreementIndexService)

    when(articleIndexService.findAllIndexes(any[String])).thenReturn(Success(List("index1", "index2")))
    when(conceptIndexService.findAllIndexes(any[String])).thenReturn(Success(List("index3", "index4")))
    when(agreementIndexService.findAllIndexes(any[String])).thenReturn(Success(List("index5", "index6")))
    doReturn(Success(""), Nil: _*).when(articleIndexService).deleteIndexWithName(Some("index1"))
    doReturn(Success(""), Nil: _*).when(articleIndexService).deleteIndexWithName(Some("index2"))
    doReturn(Success(""), Nil: _*).when(conceptIndexService).deleteIndexWithName(Some("index3"))
    doReturn(Success(""), Nil: _*).when(conceptIndexService).deleteIndexWithName(Some("index4"))
    doReturn(Success(""), Nil: _*).when(agreementIndexService).deleteIndexWithName(Some("index5"))
    doReturn(Success(""), Nil: _*).when(agreementIndexService).deleteIndexWithName(Some("index6"))

    delete("/test/index") {
      status should equal(200)
      body should equal("Deleted 6 indexes")
    }

    verify(articleIndexService).findAllIndexes(DraftApiProperties.DraftSearchIndex)
    verify(articleIndexService).deleteIndexWithName(Some("index1"))
    verify(articleIndexService).deleteIndexWithName(Some("index2"))
    verifyNoMoreInteractions(articleIndexService)

    verify(conceptIndexService).findAllIndexes(DraftApiProperties.ConceptSearchIndex)
    verify(conceptIndexService).deleteIndexWithName(Some("index3"))
    verify(conceptIndexService).deleteIndexWithName(Some("index4"))
    verifyNoMoreInteractions(conceptIndexService)

    verify(agreementIndexService).findAllIndexes(DraftApiProperties.AgreementSearchIndex)
    verify(agreementIndexService).deleteIndexWithName(Some("index5"))
    verify(agreementIndexService).deleteIndexWithName(Some("index6"))
    verifyNoMoreInteractions(agreementIndexService)
  }

  test("That DELETE /index fails if at least one index isn't found, and no indexes are deleted") {
    reset(articleIndexService)
    reset(conceptIndexService)
    reset(agreementIndexService)

    doReturn(Failure(new RuntimeException("Failed to find indexes")), Nil: _*)
      .when(articleIndexService)
      .findAllIndexes(DraftApiProperties.DraftSearchIndex)
    doReturn(Failure(new RuntimeException("Failed to find indexes")), Nil: _*)
      .when(conceptIndexService)
      .findAllIndexes(DraftApiProperties.ConceptSearchIndex)
    doReturn(Failure(new RuntimeException("Failed to find indexes")), Nil: _*)
      .when(agreementIndexService)
      .findAllIndexes(DraftApiProperties.AgreementSearchIndex)
    doReturn(Success(""), Nil: _*).when(articleIndexService).deleteIndexWithName(Some("index1"))
    doReturn(Success(""), Nil: _*).when(articleIndexService).deleteIndexWithName(Some("index2"))
    doReturn(Success(""), Nil: _*).when(conceptIndexService).deleteIndexWithName(Some("index3"))
    doReturn(Success(""), Nil: _*).when(conceptIndexService).deleteIndexWithName(Some("index4"))
    doReturn(Success(""), Nil: _*).when(agreementIndexService).deleteIndexWithName(Some("index5"))
    doReturn(Success(""), Nil: _*).when(agreementIndexService).deleteIndexWithName(Some("index6"))

    delete("/test/index") {
      status should equal(500)
      body should equal("Failed to find indexes")
    }

    verify(articleIndexService, never()).deleteIndexWithName(any[Option[String]])
    verify(conceptIndexService, never()).deleteIndexWithName(any[Option[String]])
    verify(agreementIndexService, never()).deleteIndexWithName(any[Option[String]])
  }

  test(
    "That DELETE /index fails if at least one index couldn't be deleted, but the other indexes are deleted regardless") {
    reset(articleIndexService)
    reset(conceptIndexService)
    reset(agreementIndexService)

    when(articleIndexService.findAllIndexes(any[String])).thenReturn(Success(List("index1", "index2")))
    when(conceptIndexService.findAllIndexes(any[String])).thenReturn(Success(List("index3", "index4")))
    when(agreementIndexService.findAllIndexes(any[String])).thenReturn(Success(List("index5", "index6")))

    doReturn(Success(""), Nil: _*).when(articleIndexService).deleteIndexWithName(Some("index1"))
    doReturn(Failure(new RuntimeException("No index with name 'index2' exists")), Nil: _*)
      .when(articleIndexService)
      .deleteIndexWithName(Some("index2"))
    doReturn(Success(""), Nil: _*).when(conceptIndexService).deleteIndexWithName(Some("index3"))
    doReturn(Success(""), Nil: _*).when(conceptIndexService).deleteIndexWithName(Some("index4"))
    doReturn(Success(""), Nil: _*).when(agreementIndexService).deleteIndexWithName(Some("index5"))
    doReturn(Success(""), Nil: _*).when(agreementIndexService).deleteIndexWithName(Some("index6"))

    delete("/test/index") {
      status should equal(500)
      body should equal(
        "Failed to delete 1 index: No index with name 'index2' exists. 5 indexes were deleted successfully.")
    }
    verify(articleIndexService).deleteIndexWithName(Some("index1"))
    verify(articleIndexService).deleteIndexWithName(Some("index2"))
    verify(conceptIndexService).deleteIndexWithName(Some("index3"))
    verify(conceptIndexService).deleteIndexWithName(Some("index4"))
    verify(agreementIndexService).deleteIndexWithName(Some("index5"))
    verify(agreementIndexService).deleteIndexWithName(Some("index6"))
  }
}
