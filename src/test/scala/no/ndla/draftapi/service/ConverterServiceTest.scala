/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.util.Date

import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.ArticleTitle
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.{ResourceType, TagAttributes, ValidationException}
import org.joda.time.DateTime
import org.mockito.Mockito._
import org.mockito.Matchers._

import scala.util.Success

class ConverterServiceTest extends UnitSuite with TestEnvironment {

  val service = new ConverterService

  test("toApiLicense defaults to unknown if the license was not found") {
    service.toApiLicense("invalid") should equal(api.License("unknown", None, None))
  }

  test("toApiLicense converts a short license string to a license object with description and url") {
    service.toApiLicense("by") should equal(
      api.License("by",
                  Some("Creative Commons Attribution 2.0 Generic"),
                  Some("https://creativecommons.org/licenses/by/2.0/")))
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

  test("toDomainArticle should remove PUBLISHED when merging an UpdatedArticle into an existing") {
    val existing = TestData.sampleArticleWithByNcSa.copy(status = domain.Status(DRAFT, Set(PUBLISHED)))
    val Success(res) =
      service.toDomainArticle(existing,
                              TestData.sampleApiUpdateArticle.copy(language = Some("en")),
                              isImported = false,
                              TestData.userWithWriteAccess,
                              None,
                              None)
    res.status should equal(domain.Status(DRAFT, Set.empty))

    val existing2 = TestData.sampleArticleWithByNcSa.copy(status = domain.Status(DRAFT, Set(QUEUED_FOR_PUBLISHING)))
    val Success(res2) = service.toDomainArticle(existing2,
                                                TestData.sampleApiUpdateArticle.copy(language = Some("en")),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)
    res2.status should equal(domain.Status(DRAFT, Set(QUEUED_FOR_PUBLISHING)))
  }

  test("toDomainArticle should set IMPORTED status if being imported") {
    val Success(importRes) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set())),
      TestData.sampleApiUpdateArticle,
      isImported = true,
      TestData.userWithWriteAccess,
      None,
      None
    )
    importRes.status should equal(domain.Status(DRAFT, Set(IMPORTED)))

    val Success(regularUpdate) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set(IMPORTED))),
      TestData.sampleApiUpdateArticle,
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )
    regularUpdate.status should equal(domain.Status(DRAFT, Set(IMPORTED)))
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

}
