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
import org.mockito.Matchers._
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

    delete(s"/test/article/10/", headers = Map("Authorization" -> authHeaderWithWriteRole)) {
      verify(articleApiClient, times(4)).deleteArticle(10)
    }
  }
}
