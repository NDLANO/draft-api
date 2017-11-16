/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.{DraftSwagger, TestData, TestEnvironment, UnitSuite}
import org.mockito.Mockito._
import org.scalatra.test.scalatest.ScalatraFunSuite

class AgreementControllerTest extends UnitSuite with TestEnvironment with ScalatraFunSuite {
  implicit val formats = org.json4s.DefaultFormats
  implicit val swagger = new DraftSwagger

  lazy val controller = new AgreementController
  addServlet(controller, "/test")

  val agreementId = 1

  test("/<agreement_id> should return 200 if the agreement was found") {
    when(readService.agreementWithId(1)).thenReturn(Some(TestData.sampleApiAgreement))
    get(s"/test/$agreementId") {
      status should equal(200)
    }
  }

  test("/<agreement_id> should return 404 if the article was not found") {
    when(readService.agreementWithId(agreementId)).thenReturn(None)

    get(s"/test/$agreementId") {
      status should equal(404)
    }
  }

  test("/<agreement_id> should return 400 if the request was bad") {
    get(s"/test/one") {
      status should equal(400)
    }
  }

}
