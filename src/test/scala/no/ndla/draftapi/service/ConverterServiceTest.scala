/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.model.api
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.{Attributes, ResourceType}
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
    service.toApiArticle(TestData.sampleDomainArticle, "nb") should equal(TestData.apiArticleV2)
  }

  test("toDomainArticleShould should remove unneeded attributes on embed-tags") {
    val content = s"""<h1>hello</h1><embed ${Attributes.DataResource}="${ResourceType.Image}" ${Attributes.DataUrl}="http://some-url" data-random="hehe" />"""
    val expectedContent = s"""<h1>hello</h1><embed ${Attributes.DataResource}="${ResourceType.Image}">"""
    val visualElement = s"""<embed ${Attributes.DataResource}="${ResourceType.Image}" ${Attributes.DataUrl}="http://some-url" data-random="hehe" />"""
    val expectedVisualElement = s"""<embed ${Attributes.DataResource}="${ResourceType.Image}">"""
    val apiArticle = TestData.newArticle.copy(content=Some(content), visualElement=Some(visualElement))

    when(ArticleApiClient.allocateArticleId(any[Option[String]], any[Seq[String]])).thenReturn(Success(1: Long))

    val Success(result) = service.toDomainArticle(apiArticle)
    result.content.head.content should equal (expectedContent)
    result.visualElement.head.resource should equal (expectedVisualElement)
  }

}
