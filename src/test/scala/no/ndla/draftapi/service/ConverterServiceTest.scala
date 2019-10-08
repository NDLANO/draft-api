/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.{IllegalStatusStateTransition, NewArticleMetaImage}
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.{ResourceType, TagAttributes, ValidationException}
import no.ndla.mapping.License.CC_BY
import org.joda.time.DateTime
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._
import scalikejdbc.DBSession

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

    when(draftRepository.newArticleId()(any[DBSession])).thenReturn(Success(1: Long))
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

    when(draftRepository.newArticleId()(any[DBSession])).thenReturn(Success(1: Long))

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
      service
        .updateStatus(PUBLISHED, TestData.sampleArticleWithByNcSa, TestData.userWithWriteAccess, false)
        .unsafeRunSync()
    res.getMessage should equal(
      s"Cannot go to PUBLISHED when article is ${TestData.sampleArticleWithByNcSa.status.current}")
  }

  test("stateTransitionsToApi should return no entries if user has no roles") {
    val res = service.stateTransitionsToApi(TestData.userWithNoRoles)
    res.forall { case (_, to) => to.isEmpty } should be(true)
  }

  test("stateTransitionsToApi should return only certain entries if user only has write roles") {
    val res = service.stateTransitionsToApi(TestData.userWithWriteAccess)
    res(IMPORTED.toString).length should be(1)
    res(DRAFT.toString).length should be(2)
    res(PROPOSAL.toString).length should be(5)
    res(USER_TEST.toString).length should be(4)
    res(AWAITING_QUALITY_ASSURANCE.toString).length should be(5)
    res(QUALITY_ASSURED.toString).length should be(2)
    res(QUEUED_FOR_PUBLISHING.toString).length should be(2)
    res(PUBLISHED.toString).length should be(3)
    res(AWAITING_UNPUBLISHING.toString).length should be(2)
    res(UNPUBLISHED.toString).length should be(2)
  }

  test("stateTransitionsToApi should return all entries if user is admin") {
    val res = service.stateTransitionsToApi(TestData.userWithPublishAccess)
    res(IMPORTED.toString).length should be(1)
    res(DRAFT.toString).length should be(4)
    res(PROPOSAL.toString).length should be(8)
    res(USER_TEST.toString).length should be(5)
    res(AWAITING_QUALITY_ASSURANCE.toString).length should be(6)
    res(QUALITY_ASSURED.toString).length should be(4)
    res(QUEUED_FOR_PUBLISHING.toString).length should be(3)
    res(PUBLISHED.toString).length should be(4)
    res(AWAITING_UNPUBLISHING.toString).length should be(4)
    res(UNPUBLISHED.toString).length should be(4)
  }

  test("newNotes should fail if empty strings are recieved") {
    service
      .newNotes(Seq("", "jonas"), UserInfo.apply("Kari"), Status(ArticleStatus.PROPOSAL, Set.empty))
      .isFailure should be(true)
  }

  test("Merging language fields of article should not delete not updated fields") {
    val status = Status(ArticleStatus.PUBLISHED, other = Set(ArticleStatus.IMPORTED))
    val art = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Seq(ArticleTitle("Title test", "nb")),
      content = Seq(ArticleContent("Content test", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Seq(ArticleTag(Seq("a", "b", "c"), "nb")),
      requiredLibraries = Seq(RequiredLibrary("", "", "")),
      visualElement = Seq(VisualElement("someembed", "nb")),
      introduction = Seq(ArticleIntroduction("introduction", "nb")),
      metaDescription = Seq(ArticleMetaDescription("metadesc", "nb")),
      metaImage = Seq(ArticleMetaImage("123", "metaimgalt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty
    )

    val updatedNothing = api.UpdatedArticle(
      4,
      Some("nb"),
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

    service.mergeArticleLanguageFields(art, updatedNothing, "nb") should be(art)
  }

  test("mergeArticleLanguageFields should replace every field correctly") {
    val status = Status(ArticleStatus.PUBLISHED, other = Set(ArticleStatus.IMPORTED))
    val art = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Seq(ArticleTitle("Title test", "nb")),
      content = Seq(ArticleContent("Content test", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Seq(ArticleTag(Seq("a", "b", "c"), "nb")),
      requiredLibraries = Seq(RequiredLibrary("", "", "")),
      visualElement = Seq(VisualElement("someembed", "nb")),
      introduction = Seq(ArticleIntroduction("introduction", "nb")),
      metaDescription = Seq(ArticleMetaDescription("metadesc", "nb")),
      metaImage = Seq(ArticleMetaImage("123", "metaimgalt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty
    )

    val expectedArticle = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Seq(ArticleTitle("NyTittel", "nb")),
      content = Seq(ArticleContent("NyContent", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Seq(ArticleTag(Seq("1", "2", "3"), "nb")),
      requiredLibraries = Seq(RequiredLibrary("", "", "")),
      visualElement = Seq(VisualElement("NyVisualElement", "nb")),
      introduction = Seq(ArticleIntroduction("NyIntro", "nb")),
      metaDescription = Seq(ArticleMetaDescription("NyMeta", "nb")),
      metaImage = Seq(ArticleMetaImage("321", "NyAlt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty
    )

    val updatedEverything = api.UpdatedArticle(
      revision = 4,
      language = Some("nb"),
      title = Some("NyTittel"),
      status = None,
      published = None,
      content = Some("NyContent"),
      tags = Some(Seq("1", "2", "3")),
      introduction = Some("NyIntro"),
      metaDescription = Some("NyMeta"),
      metaImage = Some(NewArticleMetaImage("321", "NyAlt")),
      visualElement = Some("NyVisualElement"),
      copyright = None,
      requiredLibraries = None,
      articleType = None,
      notes = None
    )

    service.mergeArticleLanguageFields(art, updatedEverything, "nb") should be(expectedArticle)

  }

  test("mergeArticleLanguageFields should merge every field correctly") {
    val status = Status(ArticleStatus.PUBLISHED, other = Set(ArticleStatus.IMPORTED))
    val art = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Seq(ArticleTitle("Title test", "nb")),
      content = Seq(ArticleContent("Content test", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Seq(ArticleTag(Seq("a", "b", "c"), "nb")),
      requiredLibraries = Seq(RequiredLibrary("", "", "")),
      visualElement = Seq(VisualElement("someembed", "nb")),
      introduction = Seq(ArticleIntroduction("introduction", "nb")),
      metaDescription = Seq(ArticleMetaDescription("metadesc", "nb")),
      metaImage = Seq(ArticleMetaImage("123", "metaimgalt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty
    )

    val expectedArticle = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Seq(ArticleTitle("Title test", "nb"), ArticleTitle("NyTittel", "en")),
      content = Seq(ArticleContent("Content test", "nb"), ArticleContent("NyContent", "en")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Seq(ArticleTag(Seq("a", "b", "c"), "nb"), ArticleTag(Seq("1", "2", "3"), "en")),
      requiredLibraries = Seq(RequiredLibrary("", "", "")),
      visualElement = Seq(VisualElement("someembed", "nb"), VisualElement("NyVisualElement", "en")),
      introduction = Seq(ArticleIntroduction("introduction", "nb"), ArticleIntroduction("NyIntro", "en")),
      metaDescription = Seq(ArticleMetaDescription("metadesc", "nb"), ArticleMetaDescription("NyMeta", "en")),
      metaImage = Seq(ArticleMetaImage("123", "metaimgalt", "nb"), ArticleMetaImage("321", "NyAlt", "en")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty
    )

    val updatedEverything = api.UpdatedArticle(
      revision = 4,
      language = Some("en"),
      title = Some("NyTittel"),
      status = None,
      published = None,
      content = Some("NyContent"),
      tags = Some(Seq("1", "2", "3")),
      introduction = Some("NyIntro"),
      metaDescription = Some("NyMeta"),
      metaImage = Some(NewArticleMetaImage("321", "NyAlt")),
      visualElement = Some("NyVisualElement"),
      copyright = None,
      requiredLibraries = None,
      articleType = None,
      notes = None
    )

    service.mergeArticleLanguageFields(art, updatedEverything, "en") should be(expectedArticle)

  }

  test("toDomainArticle should merge notes correctly") {
    val updatedArticleWithoutNotes =
      TestData.sampleApiUpdateArticle.copy(language = Some("nb"), title = Some("kakemonster"))
    val updatedArticleWithNotes = TestData.sampleApiUpdateArticle.copy(language = Some("nb"),
                                                                       title = Some("kakemonster"),
                                                                       notes = Some(Seq("fleibede")))
    val existingNotes = Seq(EditorNote("swoop", "", domain.Status(DRAFT, Set()), TestData.today))
    val Success(res1) =
      service.toDomainArticle(
        TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set()), notes = existingNotes),
        updatedArticleWithoutNotes,
        isImported = false,
        TestData.userWithWriteAccess,
        None,
        None
      )
    val Success(res2) =
      service.toDomainArticle(
        TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set()), notes = Seq.empty),
        updatedArticleWithoutNotes,
        isImported = false,
        TestData.userWithWriteAccess,
        None,
        None
      )
    val Success(res3) =
      service.toDomainArticle(
        TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set()), notes = existingNotes),
        updatedArticleWithNotes,
        isImported = false,
        TestData.userWithWriteAccess,
        None,
        None
      )
    val Success(res4) =
      service.toDomainArticle(
        TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set()), notes = Seq.empty),
        updatedArticleWithNotes,
        isImported = false,
        TestData.userWithWriteAccess,
        None,
        None
      )

    res1.notes should be(existingNotes)
    res2.notes should be(Seq.empty)

    res3.notes.map(_.note) should be(Seq("swoop", "fleibede"))
    res4.notes.map(_.note) should be(Seq("fleibede"))
  }

  test("Should not be able to go to ARCHIVED if published") {
    val status = Status(ArticleStatus.DRAFT, other = Set(ArticleStatus.PUBLISHED))
    val article = TestData.sampleDomainArticle.copy(status = status)
    val Failure(res: IllegalStatusStateTransition) =
      service.updateStatus(ARCHIVED, article, TestData.userWithPublishAccess, isImported = false).unsafeRunSync()

    res.getMessage should equal(s"Cannot go to ARCHIVED when article contains ${status.other}")
  }

  test("Adding new language to article will add note") {
    val updatedArticleWithoutNotes =
      TestData.sampleApiUpdateArticle.copy(title = Some("kakemonster"))
    val updatedArticleWithNotes =
      TestData.sampleApiUpdateArticle.copy(title = Some("kakemonster"), notes = Some(Seq("fleibede")))
    val existingNotes = Seq(EditorNote("swoop", "", domain.Status(DRAFT, Set()), TestData.today))
    val Success(res1) =
      service.toDomainArticle(
        TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set()), notes = existingNotes),
        updatedArticleWithNotes.copy(language = Some("sna")),
        isImported = false,
        TestData.userWithWriteAccess,
        None,
        None
      )
    val Success(res2) =
      service.toDomainArticle(
        TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set()), notes = existingNotes),
        updatedArticleWithNotes.copy(language = Some("nb")),
        isImported = false,
        TestData.userWithWriteAccess,
        None,
        None
      )
    val Success(res3) =
      service.toDomainArticle(
        TestData.sampleDomainArticle.copy(status = domain.Status(DRAFT, Set()), notes = existingNotes),
        updatedArticleWithoutNotes.copy(language = Some("sna")),
        isImported = false,
        TestData.userWithWriteAccess,
        None,
        None
      )

    res1.notes.map(_.note) should be(Seq("swoop", "fleibede", s"Ny språkvariant 'sna' ble lagt til."))
    res2.notes.map(_.note) should be(Seq("swoop", "fleibede"))
    res3.notes.map(_.note) should be(Seq("swoop", s"Ny språkvariant 'sna' ble lagt til."))

  }

}
