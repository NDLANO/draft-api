/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.api.{AccessDeniedException, ArticleApiArticle, ContentId}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.network.AuthUser
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.joda.time.DateTime
import org.mockito.ArgumentMatchers._
import org.mockito.{ArgumentCaptor, Mockito}
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import scalikejdbc.DBSession

import scala.util.{Failure, Success, Try}

class WriteServiceTest extends UnitSuite with TestEnvironment {
  override val converterService = new ConverterService

  val today = DateTime.now().toDate
  val yesterday = DateTime.now().minusDays(1).toDate
  val service = new WriteService()

  val articleId = 13
  val agreementId = 14

  val article: Article =
    TestData.sampleArticleWithPublicDomain.copy(id = Some(articleId), created = yesterday, updated = yesterday)
  val agreement: Agreement = TestData.sampleDomainAgreement.copy(id = Some(agreementId))

  override def beforeEach() = {
    Mockito.reset(articleIndexService, draftRepository, agreementIndexService, agreementRepository)

    when(draftRepository.withId(articleId)).thenReturn(Option(article))
    when(agreementRepository.withId(agreementId)).thenReturn(Option(agreement))
    when(articleIndexService.indexDocument(any[Article])).thenAnswer((invocation: InvocationOnMock) =>
      Try(invocation.getArgument[Article](0)))
    when(agreementIndexService.indexDocument(any[Agreement])).thenAnswer((invocation: InvocationOnMock) =>
      Try(invocation.getArgument[Agreement](0)))
    when(readService.addUrlsOnEmbedResources(any[Article])).thenAnswer((invocation: InvocationOnMock) =>
      invocation.getArgument[Article](0))
    when(contentValidator.validateArticle(any[Article], any[Boolean])).thenReturn(Success(article))
    when(contentValidator.validateAgreement(any[Agreement], any[Seq[ValidationMessage]])).thenReturn(Success(agreement))
    when(draftRepository.getExternalIdsFromId(any[Long])(any[DBSession])).thenReturn(List("1234"))
    when(clock.now()).thenReturn(today)
    when(draftRepository.update(any[Article], any[Boolean])(any[DBSession]))
      .thenAnswer((invocation: InvocationOnMock) => {
        val arg = invocation.getArgument[Article](0)
        Try(arg.copy(revision = Some(arg.revision.get + 1)))
      })
    when(agreementRepository.update(any[Agreement])(any[DBSession])).thenAnswer((invocation: InvocationOnMock) => {
      val arg = invocation.getArgument[Agreement](0)
      Try(arg)
    })
  }

  test("newArticle should insert a given article") {
    when(draftRepository.insert(any[Article])(any[DBSession])).thenReturn(article)
    when(draftRepository.getExternalIdsFromId(any[Long])(any[DBSession])).thenReturn(List.empty)
    when(contentValidator.validateArticle(any[Article], any[Boolean])).thenReturn(Success(article))
    when(articleApiClient.allocateArticleId(any[List[String]], any[Seq[String]])).thenReturn(Success(1: Long))

    service
      .newArticle(TestData.newArticle, List.empty, Seq.empty, TestData.userWithWriteAccess, None, None, None)
      .get
      .id
      .toString should equal(article.id.get.toString)
    verify(draftRepository, times(1)).insert(any[Article])
    verify(articleIndexService, times(1)).indexDocument(any[Article])
  }

  test("newAgreement should insert a given Agreement") {
    when(agreementRepository.insert(any[Agreement])(any[DBSession])).thenReturn(agreement)
    when(contentValidator.validateAgreement(any[Agreement], any[Seq[ValidationMessage]])).thenReturn(Success(agreement))

    service.newAgreement(TestData.newAgreement, TestData.userWithWriteAccess).get.id.toString should equal(
      agreement.id.get.toString)
    verify(agreementRepository, times(1)).insert(any[Agreement])
    verify(agreementIndexService, times(1)).indexDocument(any[Agreement])
  }

  test("That mergeLanguageFields returns original list when updated is empty") {
    val existing =
      Seq(ArticleTitle("Tittel 1", "nb"), ArticleTitle("Tittel 2", "nn"), ArticleTitle("Tittel 3", "unknown"))
    service.mergeLanguageFields(existing, Seq()) should equal(existing)
  }

  test("That mergeLanguageFields updated the english title only when specified") {
    val tittel1 = ArticleTitle("Tittel 1", "nb")
    val tittel2 = ArticleTitle("Tittel 2", "nn")
    val tittel3 = ArticleTitle("Tittel 3", "en")
    val oppdatertTittel3 = ArticleTitle("Title 3 in english", "en")

    val existing = Seq(tittel1, tittel2, tittel3)
    val updated = Seq(oppdatertTittel3)

    service.mergeLanguageFields(existing, updated) should equal(Seq(tittel1, tittel2, oppdatertTittel3))
  }

  test("That mergeLanguageFields removes a title that is empty") {
    val tittel1 = ArticleTitle("Tittel 1", "nb")
    val tittel2 = ArticleTitle("Tittel 2", "nn")
    val tittel3 = ArticleTitle("Tittel 3", "en")
    val tittelToRemove = ArticleTitle("", "nn")

    val existing = Seq(tittel1, tittel2, tittel3)
    val updated = Seq(tittelToRemove)

    service.mergeLanguageFields(existing, updated) should equal(Seq(tittel1, tittel3))
  }

  test("That mergeLanguageFields updates the title with unknown language specified") {
    val tittel1 = ArticleTitle("Tittel 1", "nb")
    val tittel2 = ArticleTitle("Tittel 2", "unknown")
    val tittel3 = ArticleTitle("Tittel 3", "en")
    val oppdatertTittel2 = ArticleTitle("Tittel 2 er oppdatert", "unknown")

    val existing = Seq(tittel1, tittel2, tittel3)
    val updated = Seq(oppdatertTittel2)

    service.mergeLanguageFields(existing, updated) should equal(Seq(tittel1, tittel3, oppdatertTittel2))
  }

  test("That mergeLanguageFields also updates the correct content") {
    val desc1 = ArticleContent("Beskrivelse 1", "nb")
    val desc2 = ArticleContent("Beskrivelse 2", "unknown")
    val desc3 = ArticleContent("Beskrivelse 3", "en")
    val oppdatertDesc2 = ArticleContent("Beskrivelse 2 er oppdatert", "unknown")

    val existing = Seq(desc1, desc2, desc3)
    val updated = Seq(oppdatertDesc2)

    service.mergeLanguageFields(existing, updated) should equal(Seq(desc1, desc3, oppdatertDesc2))
  }

  test("That updateAgreement updates only content properly") {
    val newContent = "NyContentTest"
    val updatedApiAgreement = api.UpdatedAgreement(None, Some(newContent), None)
    val expectedAgreement = agreement.copy(content = newContent, updated = today)

    service.updateAgreement(agreementId, updatedApiAgreement, TestData.userWithWriteAccess).get should equal(
      converterService.toApiAgreement(expectedAgreement))
  }

  test("That updateArticle updates only content properly") {
    val newContent = "NyContentTest"
    val updatedApiArticle =
      api.UpdatedArticle(1,
                         Some("en"),
                         None,
                         None,
                         Some(newContent),
                         None,
                         None,
                         None,
                         None,
                         None,
                         None,
                         None,
                         None,
                         None)
    val expectedArticle = article.copy(revision = Some(article.revision.get + 1),
                                       content = Seq(ArticleContent(newContent, "en")),
                                       updated = today)

    service.updateArticle(articleId,
                          updatedApiArticle,
                          List.empty,
                          Seq.empty,
                          TestData.userWithWriteAccess,
                          None,
                          None,
                          None) should equal(converterService.toApiArticle(expectedArticle, "en"))
  }

  test("That updateArticle updates only title properly") {
    val newTitle = "NyTittelTest"
    val updatedApiArticle =
      api.UpdatedArticle(1,
                         Some("en"),
                         Some(newTitle),
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
                         None)
    val expectedArticle = article.copy(revision = Some(article.revision.get + 1),
                                       title = Seq(ArticleTitle(newTitle, "en")),
                                       updated = today)

    service.updateArticle(articleId,
                          updatedApiArticle,
                          List.empty,
                          Seq.empty,
                          TestData.userWithWriteAccess,
                          None,
                          None,
                          None) should equal(converterService.toApiArticle(expectedArticle, "en"))
  }

  test("That updateArticle updates multiple fields properly") {
    val updatedTitle = "NyTittelTest"
    val updatedContent = "NyContentTest"
    val updatedTags = Seq("en", "to", "tre")
    val updatedMetaDescription = "updatedMetaHere"
    val updatedIntro = "introintro"
    val updatedMetaId = "1234"
    val updatedMetaAlt = "HeheAlt"
    val newImageMeta = api.NewArticleMetaImage(updatedMetaId, updatedMetaAlt)
    val updatedVisualElement = "<embed something>"
    val updatedCopyright = api.Copyright(Some(api.License("a", Some("b"), None)),
                                         Some("c"),
                                         Seq(api.Author("Opphavsmann", "Jonas")),
                                         List(),
                                         List(),
                                         None,
                                         None,
                                         None)
    val updatedRequiredLib = api.RequiredLibrary("tjup", "tjap", "tjim")

    val updatedApiArticle = api.UpdatedArticle(
      1,
      Some("en"),
      Some(updatedTitle),
      Some("DRAFT"),
      Some(updatedContent),
      Some(updatedTags),
      Some(updatedIntro),
      Some(updatedMetaDescription),
      Some(newImageMeta),
      Some(updatedVisualElement),
      Some(updatedCopyright),
      Some(Seq(updatedRequiredLib)),
      None,
      None
    )

    val expectedArticle = article.copy(
      revision = Some(article.revision.get + 1),
      title = Seq(ArticleTitle(updatedTitle, "en")),
      content = Seq(ArticleContent(updatedContent, "en")),
      copyright =
        Some(Copyright(Some("a"), Some("c"), Seq(Author("Opphavsmann", "Jonas")), List(), List(), None, None, None)),
      tags = Seq(ArticleTag(Seq("en", "to", "tre"), "en")),
      requiredLibraries = Seq(RequiredLibrary("tjup", "tjap", "tjim")),
      visualElement = Seq(VisualElement(updatedVisualElement, "en")),
      introduction = Seq(ArticleIntroduction(updatedIntro, "en")),
      metaDescription = Seq(ArticleMetaDescription(updatedMetaDescription, "en")),
      metaImage = Seq(ArticleMetaImage(updatedMetaId, updatedMetaAlt, "en")),
      updated = today
    )

    service.updateArticle(articleId,
                          updatedApiArticle,
                          List.empty,
                          Seq.empty,
                          TestData.userWithWriteAccess,
                          None,
                          None,
                          None) should equal(converterService.toApiArticle(expectedArticle, "en"))
  }

  test("updateArticle should use user-defined status if defined") {
    val existing = TestData.sampleDomainArticle.copy(status = TestData.statusWithDraft)
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(status = Some("PROPOSAL"))
    when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
    val Success(result) = service.updateArticle(existing.id.get,
                                                updatedArticle,
                                                List.empty,
                                                Seq.empty,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None,
                                                None)
    result.status should equal(api.Status("PROPOSAL", Seq.empty))
  }

  test(
    "updateArticle should set status to PROPOSAL if user-defined status is undefined and current status is PUBLISHEd") {
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(status = None)

    val existing = TestData.sampleDomainArticle.copy(status = TestData.statusWithPublished)
    when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
    val Success(result) = service.updateArticle(existing.id.get,
                                                updatedArticle,
                                                List.empty,
                                                Seq.empty,
                                                TestData.userWithWriteAccess,
                                                None,
                                                None,
                                                None)
    result.status should equal(api.Status("PROPOSAL", Seq.empty))
  }

  test("updateArticle should use current status if user-defined status is not set") {
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(status = None)

    {
      val existing = TestData.sampleDomainArticle.copy(status = TestData.statusWithProposal)
      when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
      val Success(result) = service.updateArticle(existing.id.get,
                                                  updatedArticle,
                                                  List.empty,
                                                  Seq.empty,
                                                  TestData.userWithWriteAccess,
                                                  None,
                                                  None,
                                                  None)
      result.status should equal(api.Status("PROPOSAL", Seq.empty))
    }

    {
      val existing = TestData.sampleDomainArticle.copy(status = TestData.statusWithUserTest)
      when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
      val Success(result) = service.updateArticle(existing.id.get,
                                                  updatedArticle,
                                                  List.empty,
                                                  Seq.empty,
                                                  TestData.userWithWriteAccess,
                                                  None,
                                                  None,
                                                  None)
      result.status should equal(api.Status("USER_TEST", Seq.empty))
    }

    {
      val existing = TestData.sampleDomainArticle.copy(status = TestData.statusWithAwaitingQA)
      when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
      val Success(result) = service.updateArticle(existing.id.get,
                                                  updatedArticle,
                                                  List.empty,
                                                  Seq.empty,
                                                  TestData.userWithWriteAccess,
                                                  None,
                                                  None,
                                                  None)
      result.status should equal(api.Status("AWAITING_QUALITY_ASSURANCE", Seq.empty))
    }

    {
      val existing = TestData.sampleDomainArticle.copy(status = TestData.statusWithQueuedForPublishing)
      when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
      val Success(result) = service.updateArticle(existing.id.get,
                                                  updatedArticle,
                                                  List.empty,
                                                  Seq.empty,
                                                  TestData.userWithWriteAccess,
                                                  None,
                                                  None,
                                                  None)
      result.status should equal(api.Status("QUEUED_FOR_PUBLISHING", Seq.empty))
    }
  }

  test("That delete article should fail when only one language") {
    val Failure(result) = service.deleteLanguage(article.id.get, "nb")
    result.getMessage should equal("Only one language left")
  }

  test("That delete article removes language from all languagefields") {
    val article =
      TestData.sampleDomainArticle.copy(title = Seq(ArticleTitle("title", "nb"), ArticleTitle("title", "nn")))
    val articleCaptor: ArgumentCaptor[Article] = ArgumentCaptor.forClass(classOf[Article])

    when(draftRepository.withId(article.id.get)).thenReturn(Some(article))
    service.deleteLanguage(article.id.get, "nn")
    verify(draftRepository).update(articleCaptor.capture(), anyBoolean())
    validateMockitoUsage()

    articleCaptor.getValue.title.length should be(1)
  }

  private def setupSuccessfulPublishMock(id: Long): Unit = {
    val article =
      TestData.sampleArticleWithByNcSa
        .copy(id = Some(id), status = domain.Status(domain.ArticleStatus.QUEUED_FOR_PUBLISHING, Set.empty))
    val apiArticle = converterService.toArticleApiArticle(article)
    when(draftRepository.update(any[Article], any[Boolean])(any[DBSession])).thenReturn(Success(article))
    when(articleApiClient.updateArticle(any[Long], any[domain.Article], any[List[String]]))
      .thenAnswer(a => {
        Success(a.getArgument(1))
      })
  }

}
