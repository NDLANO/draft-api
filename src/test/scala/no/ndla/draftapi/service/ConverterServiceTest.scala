/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.util.Date

import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.IllegalStatusStateTransition
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.ArticleTitle
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.{ResourceType, TagAttributes, ValidationException}
import no.ndla.mapping.License.{CC_BY}
import org.joda.time.DateTime
import org.mockito.Mockito._
import org.mockito.Matchers._

import scala.util.{Failure, Success}

class ConverterServiceTest extends UnitSuite with TestEnvironment {

  val service = new ConverterService

  test("toApiLicense defaults to unknown if the license was not found") {
    service.toApiLicense("invalid") should equal(api.License("unknown", None, None))
  }

  test("toApiLicense converts a short license string to a license object with description and url") {
    service.toApiLicense(CC_BY.toString) should equal(
      api.License(CC_BY.toString,
                  Some("Creative Commons Attribution 4.0 International"),
                  Some("https://creativecommons.org/licenses/by/4.0/")))
  }

  test("toApiArticle converts a domain.Article to an api.ArticleV2") {
    when(draftRepository.getExternalIdsFromId(TestData.articleId)).thenReturn(List(TestData.externalId))
    service.toApiArticle(TestData.sampleDomainArticle, "nb") should equal(Success(TestData.apiArticleV2))
  }

  test("that toApiArticle returns sorted supportedLanguages") {
    when(draftRepository.getExternalIdsFromId(TestData.articleId)).thenReturn(List(TestData.externalId))
    val result = service.toApiArticle(
      TestData.sampleDomainArticle.copy(title = TestData.sampleDomainArticle.title :+ ArticleTitle("hehe", "unknown")),
      "nb")
    result.get.supportedLanguages should be(Seq("unknown", "nb"))
  }

  test("that toApiArticleV2 returns none if article does not exist on language, and fallback is not specified") {
    when(draftRepository.getExternalIdsFromId(TestData.articleId)).thenReturn(List(TestData.externalId))
    val result = service.toApiArticle(TestData.sampleDomainArticle, "en")
    result.isFailure should be(true)
  }

  test(
    "That toApiArticleV2 returns article on existing language if fallback is specified even if selected language does not exist") {
    when(draftRepository.getExternalIdsFromId(TestData.articleId)).thenReturn(List(TestData.externalId))
    val result = service.toApiArticle(TestData.sampleDomainArticle, "en", fallback = true)
    result.get.title.get.language should be("nb")
    result.get.title.get.title should be(TestData.sampleDomainArticle.title.head.title)
    result.isFailure should be(false)
  }

  test("toDomainArticleShould should remove unneeded attributes on embed-tags") {
    val content =
      s"""<h1>hello</h1><embed ${TagAttributes.DataResource}="${ResourceType.Image}" ${TagAttributes.DataUrl}="http://some-url" data-random="hehe" />"""
    val expectedContent = s"""<h1>hello</h1><embed ${TagAttributes.DataResource}="${ResourceType.Image}">"""
    val visualElement =
      s"""<embed ${TagAttributes.DataResource}="${ResourceType.Image}" ${TagAttributes.DataUrl}="http://some-url" data-random="hehe" />"""
    val expectedVisualElement = s"""<embed ${TagAttributes.DataResource}="${ResourceType.Image}">"""
    val apiArticle = TestData.newArticle.copy(content = Some(content), visualElement = Some(visualElement))
    val expectedTime = TestData.today

    when(articleApiClient.allocateArticleId(any[List[String]], any[Seq[String]])).thenReturn(Success(1: Long))
    when(clock.now()).thenReturn(expectedTime)

    val Success(result) = service.toDomainArticle(apiArticle, List.empty, TestData.userWithWriteAccess, None, None)
    result.content.head.content should equal(expectedContent)
    result.visualElement.head.resource should equal(expectedVisualElement)
    result.created should equal(expectedTime)
    result.updated should equal(expectedTime)
  }

  test("toDomainArticleShould should use created and updated dates from parameter list if defined") {
    val apiArticle = TestData.newArticle
    val created = new DateTime("2016-12-06T16:20:05Z").toDate
    val updated = new DateTime("2017-03-07T21:18:19Z").toDate

    when(articleApiClient.allocateArticleId(any[List[String]], any[Seq[String]])).thenReturn(Success(1: Long))

    val Success(result) =
      service.toDomainArticle(apiArticle, List.empty, TestData.userWithWriteAccess, Some(created), Some(updated))
    result.created should equal(created)
    result.updated should equal(updated)
  }

  test("toDomainArticle should fail if trying to update language fields without language being set") {
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(language = None, title = Some("kakemonster"))
    val res =
      service.toDomainArticle(TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set())),
                              updatedArticle,
                              isImported = false,
                              TestData.userWithWriteAccess,
                              None,
                              None)
    res.isFailure should be(true)

    val errors = res.failed.get.asInstanceOf[ValidationException].errors
    errors.length should be(1)
    errors.head.message should equal("This field must be specified when updating language fields")
  }

  test("toDomainArticle should succeed if trying to update language fields with language being set") {
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(language = Some("nb"), title = Some("kakemonster"))
    val Success(res) =
      service.toDomainArticle(TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set())),
                              updatedArticle,
                              isImported = false,
                              TestData.userWithWriteAccess,
                              None,
                              None)
    res.title.find(_.language == "nb").get.title should equal("kakemonster")
  }

  test("updateStatus should return an IO[Failure] if the status change is illegal") {
    val Failure(res: IllegalStatusStateTransition) =
      service.updateStatus(PUBLISHED, TestData.sampleArticleWithByNcSa, TestData.userWithWriteAccess).unsafeRunSync()
    res.getMessage should equal(
      s"Cannot go to PUBLISHED when article is ${TestData.sampleArticleWithByNcSa.status.current}")
  }

  test("stateTransitionsToApi should return no entries if user has no roles") {
    val res = service.stateTransitionsToApi(TestData.userWithNoRoles)
    res.forall { case (from, to) => to.isEmpty } should be(true)
  }

  test("stateTransitionsToApi should return only certain entries if user only has write roles") {
    val res = service.stateTransitionsToApi(TestData.userWithWriteAccess)
    res(IMPORTED.toString).length should be(1)
    res(DRAFT.toString).length should be(2)
    res(PROPOSAL.toString).length should be(4)
    res(USER_TEST.toString).length should be(4)
    res(AWAITING_QUALITY_ASSURANCE.toString).length should be(4)
    res(QUALITY_ASSURED.toString).length should be(1)
    res(QUEUED_FOR_PUBLISHING.toString).length should be(1)
    res(PUBLISHED.toString).length should be(2)
    res(AWAITING_UNPUBLISHING.toString).length should be(1)
    res(UNPUBLISHED.toString).length should be(1)
  }

  test("stateTransitionsToApi should return only certain entries if user only has set_to_publish") {
    val res = service.stateTransitionsToApi(TestData.userWithPublishAccess)
    res(IMPORTED.toString).length should be(1)
    res(DRAFT.toString).length should be(2)
    res(PROPOSAL.toString).length should be(5)
    res(USER_TEST.toString).length should be(4)
    res(AWAITING_QUALITY_ASSURANCE.toString).length should be(4)
    res(QUALITY_ASSURED.toString).length should be(2)
    res(QUEUED_FOR_PUBLISHING.toString).length should be(1)
    res(PUBLISHED.toString).length should be(2)
    res(AWAITING_UNPUBLISHING.toString).length should be(1)
    res(UNPUBLISHED.toString).length should be(1)
  }

  test("stateTransitionsToApi should return all entries if user is admin") {
    val res = service.stateTransitionsToApi(TestData.userWIthAdminAccess)
    res(IMPORTED.toString).length should be(1)
    res(DRAFT.toString).length should be(3)
    res(PROPOSAL.toString).length should be(6)
    res(USER_TEST.toString).length should be(5)
    res(AWAITING_QUALITY_ASSURANCE.toString).length should be(5)
    res(QUALITY_ASSURED.toString).length should be(3)
    res(QUEUED_FOR_PUBLISHING.toString).length should be(2)
    res(PUBLISHED.toString).length should be(3)
    res(AWAITING_UNPUBLISHING.toString).length should be(3)
    res(UNPUBLISHED.toString).length should be(3)
  }

}
