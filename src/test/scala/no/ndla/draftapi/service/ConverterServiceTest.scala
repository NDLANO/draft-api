/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.util.UUID

import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.{IllegalStatusStateTransition, NewArticleMetaImage}
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.{ResourceType, TagAttributes, ValidationException}
import no.ndla.mapping.License.CC_BY
import org.joda.time.DateTime
import org.jsoup.nodes.Element
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._
import org.mockito.invocation.InvocationOnMock
import scalikejdbc.DBSession

import scala.util.{Failure, Success, Try}

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
      TestData.sampleDomainArticle.copy(title = TestData.sampleDomainArticle.title + ArticleTitle("hehe", "unknown")),
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

    when(clock.now()).thenReturn(expectedTime)

    val Success(result) = service.toDomainArticle(1, apiArticle, List.empty, TestData.userWithWriteAccess, None, None)
    result.content.head.content should equal(expectedContent)
    result.visualElement.head.resource should equal(expectedVisualElement)
    result.created should equal(expectedTime)
    result.updated should equal(expectedTime)
  }

  test("toDomainArticleShould should use created and updated dates from parameter list if defined") {
    val apiArticle = TestData.newArticle
    val created = new DateTime("2016-12-06T16:20:05Z").toDate
    val updated = new DateTime("2017-03-07T21:18:19Z").toDate

    val Success(result) =
      service.toDomainArticle(1, apiArticle, List.empty, TestData.userWithWriteAccess, Some(created), Some(updated))
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
    res(AWAITING_QUALITY_ASSURANCE.toString).length should be(6)
    res(QUALITY_ASSURED.toString).length should be(2)
    res(QUALITY_ASSURED_DELAYED.toString).length should be(2)
    res(QUEUED_FOR_PUBLISHING.toString).length should be(2)
    res(QUEUED_FOR_PUBLISHING_DELAYED.toString).length should be(2)
    res(PUBLISHED.toString).length should be(2)
    res(AWAITING_UNPUBLISHING.toString).length should be(2)
    res(UNPUBLISHED.toString).length should be(3)
  }

  test("stateTransitionsToApi should return all entries if user is admin") {
    val res = service.stateTransitionsToApi(TestData.userWithAdminAccess)
    res(IMPORTED.toString).length should be(1)
    res(DRAFT.toString).length should be(4)
    res(PROPOSAL.toString).length should be(9)
    res(USER_TEST.toString).length should be(6)
    res(AWAITING_QUALITY_ASSURANCE.toString).length should be(8)
    res(QUALITY_ASSURED.toString).length should be(6)
    res(QUALITY_ASSURED_DELAYED.toString).length should be(4)
    res(QUEUED_FOR_PUBLISHING.toString).length should be(4)
    res(QUEUED_FOR_PUBLISHING_DELAYED.toString).length should be(4)
    res(PUBLISHED.toString).length should be(5)
    res(AWAITING_UNPUBLISHING.toString).length should be(5)
    res(UNPUBLISHED.toString).length should be(5)
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
      title = Set(ArticleTitle("Title test", "nb")),
      content = Set(ArticleContent("Content test", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Set(ArticleTag(Seq("a", "b", "c"), "nb")),
      requiredLibraries = Set(RequiredLibrary("", "", "")),
      visualElement = Set(VisualElement("someembed", "nb")),
      introduction = Set(ArticleIntroduction("introduction", "nb")),
      metaDescription = Set(ArticleMetaDescription("metadesc", "nb")),
      metaImage = Set(ArticleMetaImage("123", "metaimgalt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty,
      editorLabels = Seq.empty,
      grepCodes = Seq.empty,
      conceptIds = Seq.empty,
      availability = Availability.everyone,
      relatedContent = Seq.empty
    )

    val updatedNothing = TestData.blankUpdatedArticle.copy(
      revision = 4,
      language = Some("nb")
    )

    service.mergeArticleLanguageFields(art, updatedNothing, "nb") should be(art)
  }

  test("mergeArticleLanguageFields should replace every field correctly") {
    val status = Status(ArticleStatus.PUBLISHED, other = Set(ArticleStatus.IMPORTED))
    val art = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Set(ArticleTitle("Title test", "nb")),
      content = Set(ArticleContent("Content test", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Set(ArticleTag(Seq("a", "b", "c"), "nb")),
      requiredLibraries = Set(RequiredLibrary("", "", "")),
      visualElement = Set(VisualElement("someembed", "nb")),
      introduction = Set(ArticleIntroduction("introduction", "nb")),
      metaDescription = Set(ArticleMetaDescription("metadesc", "nb")),
      metaImage = Set(ArticleMetaImage("123", "metaimgalt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty,
      editorLabels = Seq.empty,
      grepCodes = Seq.empty,
      conceptIds = Seq.empty,
      availability = Availability.everyone,
      relatedContent = Seq.empty
    )

    val expectedArticle = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Set(ArticleTitle("NyTittel", "nb")),
      content = Set(ArticleContent("NyContent", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Set(ArticleTag(Seq("1", "2", "3"), "nb")),
      requiredLibraries = Set(RequiredLibrary("", "", "")),
      visualElement = Set(VisualElement("NyVisualElement", "nb")),
      introduction = Set(ArticleIntroduction("NyIntro", "nb")),
      metaDescription = Set(ArticleMetaDescription("NyMeta", "nb")),
      metaImage = Set(ArticleMetaImage("321", "NyAlt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty,
      editorLabels = Seq.empty,
      grepCodes = Seq.empty,
      conceptIds = Seq.empty,
      availability = Availability.everyone,
      relatedContent = Seq.empty
    )

    val updatedEverything = TestData.blankUpdatedArticle.copy(
      revision = 4,
      language = Some("nb"),
      title = Some("NyTittel"),
      status = None,
      published = None,
      content = Some("NyContent"),
      tags = Some(Seq("1", "2", "3")),
      introduction = Some("NyIntro"),
      metaDescription = Some("NyMeta"),
      metaImage = Right(Some(NewArticleMetaImage("321", "NyAlt"))),
      visualElement = Some("NyVisualElement"),
      copyright = None,
      requiredLibraries = None,
      articleType = None,
      notes = None,
      editorLabels = None,
      grepCodes = None,
      conceptIds = None,
      createNewVersion = None
    )

    service.mergeArticleLanguageFields(art, updatedEverything, "nb") should be(expectedArticle)

  }

  test("mergeArticleLanguageFields should merge every field correctly") {
    val status = Status(ArticleStatus.PUBLISHED, other = Set(ArticleStatus.IMPORTED))
    val art = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Set(ArticleTitle("Title test", "nb")),
      content = Set(ArticleContent("Content test", "nb")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Set(ArticleTag(Seq("a", "b", "c"), "nb")),
      requiredLibraries = Set(RequiredLibrary("", "", "")),
      visualElement = Set(VisualElement("someembed", "nb")),
      introduction = Set(ArticleIntroduction("introduction", "nb")),
      metaDescription = Set(ArticleMetaDescription("metadesc", "nb")),
      metaImage = Set(ArticleMetaImage("123", "metaimgalt", "nb")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty,
      editorLabels = Seq.empty,
      grepCodes = Seq.empty,
      conceptIds = Seq.empty,
      availability = Availability.everyone,
      relatedContent = Seq.empty
    )

    val expectedArticle = Article(
      id = Some(3),
      revision = Some(4),
      status = status,
      title = Set(ArticleTitle("Title test", "nb"), ArticleTitle("NyTittel", "en")),
      content = Set(ArticleContent("Content test", "nb"), ArticleContent("NyContent", "en")),
      copyright = TestData.sampleArticleWithByNcSa.copyright,
      tags = Set(ArticleTag(Seq("a", "b", "c"), "nb"), ArticleTag(Seq("1", "2", "3"), "en")),
      requiredLibraries = Set(RequiredLibrary("", "", "")),
      visualElement = Set(VisualElement("someembed", "nb"), VisualElement("NyVisualElement", "en")),
      introduction = Set(ArticleIntroduction("introduction", "nb"), ArticleIntroduction("NyIntro", "en")),
      metaDescription = Set(ArticleMetaDescription("metadesc", "nb"), ArticleMetaDescription("NyMeta", "en")),
      metaImage = Set(ArticleMetaImage("123", "metaimgalt", "nb"), ArticleMetaImage("321", "NyAlt", "en")),
      created = TestData.today,
      updated = TestData.today,
      updatedBy = "theuserthatchangeditid",
      published = TestData.today,
      articleType = ArticleType.Standard,
      notes = Seq(EditorNote("Note here", "sheeps", status, TestData.today)),
      previousVersionsNotes = Seq.empty,
      editorLabels = Seq.empty,
      grepCodes = Seq.empty,
      conceptIds = Seq.empty,
      availability = Availability.everyone,
      relatedContent = Seq.empty
    )

    val updatedEverything = TestData.blankUpdatedArticle.copy(
      revision = 4,
      language = Some("en"),
      title = Some("NyTittel"),
      status = None,
      published = None,
      content = Some("NyContent"),
      tags = Some(Seq("1", "2", "3")),
      introduction = Some("NyIntro"),
      metaDescription = Some("NyMeta"),
      metaImage = Right(Some(NewArticleMetaImage("321", "NyAlt"))),
      visualElement = Some("NyVisualElement"),
      copyright = None,
      requiredLibraries = None,
      articleType = None,
      notes = None,
      editorLabels = None,
      grepCodes = None,
      conceptIds = None,
      createNewVersion = None
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

  test("toDomainArticle(NewArticle) should convert grepCodes correctly") {
    val Success(res1) = service.toDomainArticle(1,
                                                TestData.newArticle.copy(grepCodes = Seq("a", "b")),
                                                List(TestData.externalId),
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    val Success(res2) = service.toDomainArticle(1,
                                                TestData.newArticle.copy(grepCodes = Seq.empty),
                                                List(TestData.externalId),
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    res1.grepCodes should be(Seq("a", "b"))
    res2.grepCodes should be(Seq.empty)
  }

  test("toDomainArticle(UpdateArticle) should convert grepCodes correctly") {
    val Success(res1) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(grepCodes = Seq("a", "b", "c")),
      TestData.sampleApiUpdateArticle.copy(grepCodes = Some(Seq("x", "y"))),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    val Success(res2) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(grepCodes = Seq("a", "b", "c")),
      TestData.sampleApiUpdateArticle.copy(grepCodes = Some(Seq.empty)),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    val Success(res3) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(grepCodes = Seq("a", "b", "c")),
      TestData.sampleApiUpdateArticle.copy(grepCodes = None),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    res1.grepCodes should be(Seq("x", "y"))
    res2.grepCodes should be(Seq.empty)
    res3.grepCodes should be(Seq("a", "b", "c"))
  }

  test("toDomainArticle(updateNullDocumentArticle) should convert grepCodes correctly") {

    val Success(res1) = service.toDomainArticle(1,
                                                TestData.sampleApiUpdateArticle.copy(grepCodes = Some(Seq("a", "b"))),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    val Success(res2) = service.toDomainArticle(2,
                                                TestData.sampleApiUpdateArticle.copy(grepCodes = Some(Seq.empty)),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    val Success(res3) = service.toDomainArticle(3,
                                                TestData.sampleApiUpdateArticle.copy(grepCodes = None),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    res1.grepCodes should be(Seq("a", "b"))
    res2.grepCodes should be(Seq.empty)
    res3.grepCodes should be(Seq.empty)
  }

  test("toDomainArticle(UpdateArticle) should update metaImage correctly") {

    val beforeUpdate = TestData.sampleDomainArticle.copy(
      metaImage = Set(domain.ArticleMetaImage("1", "Hei", "nb"), domain.ArticleMetaImage("2", "Hej", "nn")))

    val Success(res1) = service.toDomainArticle(
      beforeUpdate,
      TestData.sampleApiUpdateArticle.copy(language = Some("nb"), metaImage = Left(null)),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    val Success(res2) = service.toDomainArticle(
      beforeUpdate,
      TestData.sampleApiUpdateArticle.copy(language = Some("nb"),
                                           metaImage = Right(Some(api.NewArticleMetaImage("1", "Hola")))),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    val Success(res3) = service.toDomainArticle(
      beforeUpdate,
      TestData.sampleApiUpdateArticle.copy(language = Some("nb"), metaImage = Right(None)),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    res1.metaImage should be(Set(domain.ArticleMetaImage("2", "Hej", "nn")))
    res2.metaImage should be(Set(domain.ArticleMetaImage("2", "Hej", "nn"), domain.ArticleMetaImage("1", "Hola", "nb")))
    res3.metaImage should be(beforeUpdate.metaImage)
  }

  test("toDomainArticle(updateNullDocumentArticle) should update metaImage correctly") {

    val Success(res1) = service.toDomainArticle(1,
                                                TestData.sampleApiUpdateArticle.copy(metaImage = Left(null)),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    val Success(res2) = service.toDomainArticle(
      2,
      TestData.sampleApiUpdateArticle.copy(metaImage = Right(Some(api.NewArticleMetaImage("1", "Hola")))),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    val Success(res3) = service.toDomainArticle(3,
                                                TestData.sampleApiUpdateArticle.copy(metaImage = Right(None)),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    res1.metaImage should be(Set.empty)
    res2.metaImage should be(Set(domain.ArticleMetaImage("1", "Hola", "nb")))
    res3.metaImage should be(Set.empty)
  }

  test("toDomainArticle should clone files if existing files appear in new language") {
    val embed1 =
      """<embed data-alt="Kul alt1" data-path="/files/resources/abc123.pdf" data-resource="file" data-title="Kul tittel1" data-type="pdf">"""
    val existingArticle = TestData.sampleDomainArticle.copy(
      content = Set(domain.ArticleContent(s"<section><h1>Hei</h1>$embed1</section>", "nb"))
    )

    val newContent = s"<section><h1>Hello</h1>$embed1</section>"
    val apiArticle = TestData.blankUpdatedArticle.copy(
      language = Some("en"),
      title = Some("Eng title"),
      content = Some(newContent)
    )

    when(writeService.cloneEmbedAndUpdateElement(any[Element])).thenAnswer((i: InvocationOnMock) => {
      val element = i.getArgument[Element](0)
      Success(element.attr("data-path", "/files/resources/new.pdf"))
    })

    val Success(res1) = service.toDomainArticle(
      existingArticle,
      apiArticle,
      false,
      TestData.userWithWriteAccess,
      None,
      None
    )

  }

  test("Extracting h5p paths works as expected") {
    val enPath1 = s"/resources/${UUID.randomUUID().toString}"
    val enPath2 = s"/resources/${UUID.randomUUID().toString}"
    val nbPath1 = s"/resources/${UUID.randomUUID().toString}"
    val nbPath2 = s"/resources/${UUID.randomUUID().toString}"
    val vePath1 = s"/resources/${UUID.randomUUID().toString}"
    val vePath2 = s"/resources/${UUID.randomUUID().toString}"

    val expectedPaths = Set(enPath1, enPath2, nbPath1, nbPath2, vePath1, vePath2)

    val articleContentNb = domain.ArticleContent(
      s"""<section><h1>Heisann</h1><embed data-path="$nbPath1" data-resource="h5p" /></section><section><p>Joda<embed data-path="$nbPath2" data-resource="h5p" /></p><embed data-resource="concept" data-path="thisisinvalidbutletsdoit"/></section>""",
      "nb"
    )
    val articleContentEn = domain.ArticleContent(
      s"""<section><h1>Hello</h1><embed data-path="$enPath1" data-resource="h5p" /></section><section><p>Joda<embed data-path="$enPath2" data-resource="h5p" /></p><embed data-resource="concept" data-path="thisisinvalidbutletsdoit"/></section>""",
      "en"
    )

    val visualElementNb = domain.VisualElement(s"""<embed data-path="$vePath1" data-resource="h5p" />""", "nb")
    val visualElementEn = domain.VisualElement(s"""<embed data-path="$vePath2" data-resource="h5p" />""", "en")

    val article = TestData.sampleDomainArticle.copy(id = Some(1),
                                                    content = Set(articleContentNb, articleContentEn),
                                                    visualElement = Set(visualElementNb, visualElementEn))
    service.getEmbeddedH5PPaths(article) should be(expectedPaths)
  }

  test("toDomainArticle(NewArticle) should convert availability correctly") {
    val Success(res1) =
      service.toDomainArticle(1,
                              TestData.newArticle.copy(availability = Some(Availability.teacher.toString)),
                              List(TestData.externalId),
                              TestData.userWithWriteAccess,
                              None,
                              None)

    val Success(res2) = service.toDomainArticle(1,
                                                TestData.newArticle.copy(availability = None),
                                                List(TestData.externalId),
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    val Success(res3) = service.toDomainArticle(1,
                                                TestData.newArticle.copy(availability = Some("Krutte go")),
                                                List(TestData.externalId),
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    res1.availability should be(Availability.teacher)
    res1.availability should not be (Availability.everyone)
    // Should default til everyone
    res2.availability should be(Availability.everyone)
    res3.availability should be(Availability.everyone)
  }

  test("toDomainArticle(UpdateArticle) should convert availability correctly") {
    val Success(res1) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(availability = Availability.student),
      TestData.sampleApiUpdateArticle.copy(availability = Some(Availability.teacher.toString)),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    val Success(res2) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(availability = Availability.student),
      TestData.sampleApiUpdateArticle.copy(availability = Some("Krutte go")),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    val Success(res3) = service.toDomainArticle(
      TestData.sampleDomainArticle.copy(availability = Availability.student),
      TestData.sampleApiUpdateArticle.copy(availability = None),
      isImported = false,
      TestData.userWithWriteAccess,
      None,
      None
    )

    res1.availability should be(Availability.teacher)
    res2.availability should be(Availability.student)
    res3.availability should be(Availability.student)
  }

  test("toDomainArticle(updateNullDocumentArticle) should convert availability correctly") {

    val Success(res1) =
      service.toDomainArticle(1,
                              TestData.sampleApiUpdateArticle.copy(availability = Some(Availability.student.toString)),
                              isImported = false,
                              TestData.userWithWriteAccess,
                              None,
                              None)

    val Success(res2) = service.toDomainArticle(2,
                                                TestData.sampleApiUpdateArticle.copy(availability = Some("Krutte go")),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    val Success(res3) = service.toDomainArticle(3,
                                                TestData.sampleApiUpdateArticle.copy(availability = None),
                                                isImported = false,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None)

    res1.availability should be(Availability.student)
    res2.availability should be(Availability.everyone)
    res3.availability should be(Availability.everyone)
  }

  test("toDomainArticle should convert relatedContent correctly") {

    val Success(res1) =
      service.toDomainArticle(1,
                              TestData.sampleApiUpdateArticle.copy(relatedContent = Some(List(Right(1)))),
                              isImported = false,
                              TestData.userWithWriteAccess,
                              None,
                              None)

    res1.relatedContent should be(List(Right(1L)))
  }

}
