/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.ArticleTitle
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.{ResourceType, TagAttributes, ValidationException}
import org.mockito.Mockito._
import org.mockito.Matchers._

import scala.util.Success

class ConverterServiceTest extends UnitSuite with TestEnvironment {

  val service = new ConverterService

  test("toApiLicense defaults to unknown if the license was not found") {
    service.toApiLicense("invalid") should equal(api.License("unknown", None, None))
  }

  test("toApiLicense converts a short license string to a license object with description and url") {
    service.toApiLicense("by") should equal(api.License("by", Some("Creative Commons Attribution 2.0 Generic"), Some("https://creativecommons.org/licenses/by/2.0/")))
  }

  test("toApiArticle converts a domain.Article to an api.ArticleV2") {
    when(draftRepository.getExternalIdFromId(TestData.articleId)).thenReturn(Some(TestData.externalId))
    service.toApiArticle(TestData.sampleDomainArticle, "nb") should equal(Success(TestData.apiArticleV2))
  }

  test("that toApiArticle returns sorted supportedLanguages") {
    when(draftRepository.getExternalIdFromId(TestData.articleId)).thenReturn(Some(TestData.externalId))
    val result = service.toApiArticle(TestData.sampleDomainArticle.copy(title = TestData.sampleDomainArticle.title :+ ArticleTitle("hehe", "unknown")), "nb")
    result.get.supportedLanguages should be(Seq("unknown", "nb"))
  }

  test("that toApiArticleV2 returns none if article does not exist on language, and fallback is not specified") {
    when(draftRepository.getExternalIdFromId(TestData.articleId)).thenReturn(Some(TestData.externalId))
    val result = service.toApiArticle(TestData.sampleDomainArticle, "en")
    result.isFailure should be (true)
  }

  test("That toApiArticleV2 returns article on existing language if fallback is specified even if selected language does not exist") {
    when(draftRepository.getExternalIdFromId(TestData.articleId)).thenReturn(Some(TestData.externalId))
    val result = service.toApiArticle(TestData.sampleDomainArticle, "en", fallback = true)
    result.get.title.get.language should be("nb")
    result.get.title.get.title should be(TestData.sampleDomainArticle.title.head.title)
    result.isFailure should be (false)
  }

  test("toDomainArticleShould should remove unneeded attributes on embed-tags") {
    val content = s"""<h1>hello</h1><embed ${TagAttributes.DataResource}="${ResourceType.Image}" ${TagAttributes.DataUrl}="http://some-url" data-random="hehe" />"""
    val expectedContent = s"""<h1>hello</h1><embed ${TagAttributes.DataResource}="${ResourceType.Image}">"""
    val visualElement = s"""<embed ${TagAttributes.DataResource}="${ResourceType.Image}" ${TagAttributes.DataUrl}="http://some-url" data-random="hehe" />"""
    val expectedVisualElement = s"""<embed ${TagAttributes.DataResource}="${ResourceType.Image}">"""
    val apiArticle = TestData.newArticle.copy(content=Some(content), visualElement=Some(visualElement))

    when(articleApiClient.allocateArticleId(any[Option[String]], any[Seq[String]])).thenReturn(Success(1: Long))

    val Success(result) = service.toDomainArticle(apiArticle, None)
    result.content.head.content should equal (expectedContent)
    result.visualElement.head.resource should equal (expectedVisualElement)
  }

  test("toDomainArticle should remove PUBLISHED when merging an UpdatedArticle into an existing") {
    val existing = TestData.sampleArticleWithByNcSa.copy(status = Set(DRAFT, PUBLISHED))
    val Success(res) = service.toDomainArticle(existing, TestData.sampleApiUpdateArticle.copy(language = Some("en")), isImported = false
    )
    res.status should equal(Set(DRAFT))

    val existing2 = TestData.sampleArticleWithByNcSa.copy(status = Set(CREATED, QUEUED_FOR_PUBLISHING))
    val Success(res2) = service.toDomainArticle(existing2, TestData.sampleApiUpdateArticle.copy(language = Some("en")), isImported = false)
    res2.status should equal(Set(DRAFT, QUEUED_FOR_PUBLISHING))
  }

  test("toDomainArticle should set IMPORTED status if being imported") {
    val Success(importRes) = service.toDomainArticle(TestData.sampleDomainArticle.copy(status=Set()), TestData.sampleApiUpdateArticle, isImported = true)
    importRes.status should equal(Set(IMPORTED))

    val Success(regularUpdate) = service.toDomainArticle(TestData.sampleDomainArticle.copy(status=Set(IMPORTED)), TestData.sampleApiUpdateArticle, isImported = false)
    regularUpdate.status should equal(Set(IMPORTED, DRAFT))
  }

  test("toDomainArticle should fail if trying to update language fields without language being set") {
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(language=None, title=Some("kakemonster"))
    val res = service.toDomainArticle(TestData.sampleDomainArticle.copy(status=Set()), updatedArticle, isImported = false)
    res.isFailure should be (true)

    val errors = res.failed.get.asInstanceOf[ValidationException].errors
    errors.length should be (1)
    errors.head.message should equal ("This field must be specified when updating language fields")
  }

  test("toDomainArticle should succeed if trying to update language fields with language being set") {
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(language=Some("nb"), title=Some("kakemonster"))
    val Success(res) = service.toDomainArticle(TestData.sampleDomainArticle.copy(status=Set()), updatedArticle, isImported = false)
    res.title.find(_.language == "nb").get.title should equal("kakemonster")
  }

}
