/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.model.domain.{SearchResult, Sort}
import no.ndla.draftapi.model.api
import no.ndla.draftapi.{DraftSwagger, TestData, TestEnvironment, UnitSuite}
import org.json4s.DefaultFormats
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.scalatra.test.scalatest.ScalatraFunSuite

import scala.util.Success

class ConceptControllerTest extends UnitSuite with TestEnvironment with ScalatraFunSuite {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  implicit val swagger: DraftSwagger = new DraftSwagger

  lazy val controller = new ConceptController
  addServlet(controller, "/test")

  val conceptId = 1
  val lang = "nb"

  test("/<concept_id> should return 200 if the cover was found") {
    when(readService.conceptWithId(1, lang)).thenReturn(Some(TestData.sampleApiConcept))
    get(s"/test/$conceptId?language=$lang") {
      status should equal(200)
    }
  }

  test("/<concept_id> should return 404 if the article was not found") {
    when(readService.conceptWithId(conceptId, lang)).thenReturn(None)

    get(s"/test/$conceptId?language=$lang") {
      status should equal(404)
    }
  }

  test("/<concept_id> should return 400 if the article was not found") {
    get(s"/test/one") {
      status should equal(400)
    }
  }

  test("That scrollId is in header, and not in body") {
    val scrollId =
      "DnF1ZXJ5VGhlbkZldGNoCgAAAAAAAAC1Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAthYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALcWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC4Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuRYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALsWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC9Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuhYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAAL4WLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC8Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFE="
    val searchResponse = SearchResult[api.ConceptSummary](
      0,
      Some(1),
      10,
      "nb",
      Seq.empty[api.ConceptSummary],
      Some(scrollId)
    )
    when(conceptSearchService.all(any[List[Long]], any[String], any[Int], any[Int], any[Sort.Value], any[Boolean]))
      .thenReturn(Success(searchResponse))

    get(s"/test/") {
      status should be(200)
      body.contains(scrollId) should be(false)
      header("search-context") should be(scrollId)
    }
  }

  test("That scrolling uses scroll and not searches normally") {
    reset(conceptSearchService)
    val scrollId =
      "DnF1ZXJ5VGhlbkZldGNoCgAAAAAAAAC1Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAthYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALcWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC4Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuRYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALsWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC9Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuhYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAAL4WLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC8Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFE="
    val searchResponse = SearchResult[api.ConceptSummary](
      0,
      Some(1),
      10,
      "nb",
      Seq.empty[api.ConceptSummary],
      Some(scrollId)
    )

    when(conceptSearchService.scroll(anyString, anyString)).thenReturn(Success(searchResponse))

    get(s"/test?search-context=$scrollId") {
      status should be(200)
    }

    verify(conceptSearchService, times(0)).all(any[List[Long]],
                                               any[String],
                                               any[Int],
                                               any[Int],
                                               any[Sort.Value],
                                               any[Boolean])
    verify(conceptSearchService, times(0)).matchingQuery(any[String],
                                                         any[List[Long]],
                                                         any[String],
                                                         any[Int],
                                                         any[Int],
                                                         any[Sort.Value],
                                                         any[Boolean])
    verify(conceptSearchService, times(1)).scroll(eqTo(scrollId), any[String])
  }

  test("That scrolling with POST uses scroll and not searches normally") {
    reset(conceptSearchService)
    val scrollId =
      "DnF1ZXJ5VGhlbkZldGNoCgAAAAAAAAC1Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAthYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALcWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC4Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuRYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAALsWLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC9Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFEAAAAAAAAAuhYtY2VPYWFvRFQ5aWNSbzRFYVZSTEhRAAAAAAAAAL4WLWNlT2Fhb0RUOWljUm80RWFWUkxIUQAAAAAAAAC8Fi1jZU9hYW9EVDlpY1JvNEVhVlJMSFE="
    val searchResponse = SearchResult[api.ConceptSummary](
      0,
      Some(1),
      10,
      "nb",
      Seq.empty[api.ConceptSummary],
      Some(scrollId)
    )

    when(conceptSearchService.scroll(anyString, anyString)).thenReturn(Success(searchResponse))

    post(s"/test/search/?search-context=$scrollId") {
      status should be(200)
    }

    verify(conceptSearchService, times(0)).all(any[List[Long]],
                                               any[String],
                                               any[Int],
                                               any[Int],
                                               any[Sort.Value],
                                               any[Boolean])
    verify(conceptSearchService, times(0)).matchingQuery(any[String],
                                                         any[List[Long]],
                                                         any[String],
                                                         any[Int],
                                                         any[Int],
                                                         any[Sort.Value],
                                                         any[Boolean])
    verify(conceptSearchService, times(1)).scroll(eqTo(scrollId), any[String])
  }

}
