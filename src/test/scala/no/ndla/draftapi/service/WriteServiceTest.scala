/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.io.ByteArrayInputStream
import java.util.Date

import no.ndla.draftapi.auth.{Role, UserInfo}
import no.ndla.draftapi.model.api.ArticleApiArticle
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite, integration}
import no.ndla.validation.{HtmlTagRules, ValidationMessage}
import org.joda.time.{DateTime, DateTimeUtils}
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.mockito.{ArgumentCaptor, Mockito}
import org.scalatra.servlet.FileItem
import scalikejdbc.DBSession

import scala.util.{Failure, Success, Try}

class WriteServiceTest extends UnitSuite with TestEnvironment {
  override val converterService = new ConverterService

  val today: Date = DateTime.now().toDate
  val yesterday: Date = DateTime.now().minusDays(1).toDate
  val service = new WriteService()

  val articleId = 13
  val agreementId = 14

  val article: Article =
    TestData.sampleArticleWithPublicDomain.copy(id = Some(articleId), created = yesterday, updated = yesterday)

  val topicArticle: Article =
    TestData.sampleTopicArticle.copy(id = Some(articleId), created = yesterday, updated = yesterday)
  val agreement: Agreement = TestData.sampleDomainAgreement.copy(id = Some(agreementId))

  override def beforeEach(): Unit = {
    Mockito.reset(articleIndexService, draftRepository, agreementIndexService, agreementRepository, contentValidator)

    when(draftRepository.withId(articleId)).thenReturn(Option(article))
    when(agreementRepository.withId(agreementId)).thenReturn(Option(agreement))
    when(articleIndexService.indexDocument(any[Article])).thenAnswer((invocation: InvocationOnMock) =>
      Try(invocation.getArgument[Article](0)))
    when(tagIndexService.indexDocument(any[Article])).thenAnswer((invocation: InvocationOnMock) =>
      Try(invocation.getArgument[Article](0)))
    when(agreementIndexService.indexDocument(any[Agreement])).thenAnswer((invocation: InvocationOnMock) =>
      Try(invocation.getArgument[Agreement](0)))
    when(readService.addUrlsOnEmbedResources(any[Article])).thenAnswer((invocation: InvocationOnMock) =>
      invocation.getArgument[Article](0))
    when(contentValidator.validateArticle(any[Article], any[Boolean])).thenReturn(Success(article))
    when(contentValidator.validateAgreement(any[Agreement], any[Seq[ValidationMessage]])).thenReturn(Success(agreement))
    when(draftRepository.getExternalIdsFromId(any[Long])(any[DBSession])).thenReturn(List("1234"))
    when(clock.now()).thenReturn(today)
    when(draftRepository.updateArticle(any[Article], any[Boolean])(any[DBSession]))
      .thenAnswer((invocation: InvocationOnMock) => {
        val arg = invocation.getArgument[Article](0)
        Try(arg.copy(revision = Some(arg.revision.getOrElse(0) + 1)))
      })
    when(draftRepository.storeArticleAsNewVersion(any[Article])(any[DBSession]))
      .thenAnswer((invocation: InvocationOnMock) => {
        val arg = invocation.getArgument[Article](0)
        Try(arg.copy(revision = Some(arg.revision.getOrElse(0) + 1)))
      })

    when(agreementRepository.update(any[Agreement])(any[DBSession])).thenAnswer((invocation: InvocationOnMock) => {
      val arg = invocation.getArgument[Agreement](0)
      Try(arg)
    })
    when(taxonomyApiClient.updateTaxonomyIfExists(any[Long], any[domain.Article])).thenAnswer((i: InvocationOnMock) => {
      Success(i.getArgument[Long](0))
    })
  }

  test("newArticle should insert a given article") {
    when(draftRepository.getExternalIdsFromId(any[Long])(any[DBSession])).thenReturn(List.empty)
    when(contentValidator.validateArticle(any[Article], any[Boolean])).thenReturn(Success(article))
    when(draftRepository.newEmptyArticle(any[List[String]], any[List[String]])(any[DBSession]))
      .thenReturn(Success(1: Long))

    service
      .newArticle(TestData.newArticle, List.empty, Seq.empty, TestData.userWithWriteAccess, None, None, None)
      .isSuccess should be(true)

    verify(draftRepository, times(1)).newEmptyArticle()
    verify(draftRepository, times(0)).insert(any[Article])
    verify(draftRepository, times(1)).updateArticle(any[Article], any[Boolean])
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
    val updatedApiArticle = TestData.blankUpdatedArticle.copy(
      revision = 1,
      language = Some("en"),
      content = Some(newContent)
    )
    val expectedArticle =
      article.copy(revision = Some(article.revision.get + 1),
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
    val updatedApiArticle = TestData.blankUpdatedArticle.copy(
      revision = 1,
      language = Some("en"),
      title = Some(newTitle)
    )
    val expectedArticle =
      article.copy(revision = Some(article.revision.get + 1),
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
    val updatedPublishedDate = yesterday
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
    val updatedArticleType = "topic-article"

    val updatedApiArticle = TestData.blankUpdatedArticle.copy(
      revision = 1,
      language = Some("en"),
      title = Some(updatedTitle),
      status = Some("DRAFT"),
      published = Some(updatedPublishedDate),
      content = Some(updatedContent),
      tags = Some(updatedTags),
      introduction = Some(updatedIntro),
      metaDescription = Some(updatedMetaDescription),
      metaImage = Right(Some(newImageMeta)),
      visualElement = Some(updatedVisualElement),
      copyright = Some(updatedCopyright),
      requiredLibraries = Some(Seq(updatedRequiredLib)),
      articleType = Some(updatedArticleType)
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
      updated = today,
      published = yesterday,
      articleType = ArticleType.TopicArticle
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
    "updateArticle should set status to PROPOSAL if user-defined status is undefined and current status is PUBLISHED") {
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
    result.status should equal(api.Status("PROPOSAL", Seq("PUBLISHED")))
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
      when(contentValidator.validateArticle(any[Article], any[Boolean])).thenReturn(Success(existing))
      when(articleApiClient.validateArticle(any[ArticleApiArticle], any[Boolean])).thenAnswer((i: InvocationOnMock) => {
        Success(i.getArgument[ArticleApiArticle](0))
      })
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
    val Failure(result) = service.deleteLanguage(article.id.get, "nb", UserInfo("asdf", Set()))
    result.getMessage should equal("Only one language left")
  }

  test("That delete article removes language from all languagefields") {
    val article =
      TestData.sampleDomainArticle.copy(id = Some(3),
                                        title = Seq(ArticleTitle("title", "nb"), ArticleTitle("title", "nn")))
    val articleCaptor: ArgumentCaptor[Article] = ArgumentCaptor.forClass(classOf[Article])

    when(draftRepository.withId(anyLong)).thenReturn(Some(article))
    service.deleteLanguage(article.id.get, "nn", UserInfo("asdf", Set()))
    verify(draftRepository).updateArticle(articleCaptor.capture(), anyBoolean)

    articleCaptor.getValue.title.length should be(1)
  }

  test("That get file extension will split extension and name as expected") {
    val a = "test.pdf"
    val b = "test"
    val c = "te.st.csv"
    val d = ".te....st.txt"
    val e = "kek.jpeg"

    service.getFileExtension(a) should be(Success(".pdf"))
    service.getFileExtension(b).isFailure should be(true)
    service.getFileExtension(c) should be(Success(".csv"))
    service.getFileExtension(d) should be(Success(".txt"))
    service.getFileExtension(e).isFailure should be(true)
  }

  test("uploading file calls fileStorageService as expected") {
    val fileToUpload = mock[FileItem]
    val fileBytes: Array[Byte] = "these are not the bytes you're looking for".getBytes
    when(fileToUpload.get()).thenReturn(fileBytes)
    when(fileToUpload.size).thenReturn(fileBytes.length)
    when(fileToUpload.getContentType).thenReturn(Some("application/pdf"))
    when(fileToUpload.name).thenReturn("myfile.pdf")
    when(fileStorage.resourceExists(anyString())).thenReturn(false)
    when(
      fileStorage
        .uploadResourceFromStream(any[ByteArrayInputStream],
                                  anyString(),
                                  eqTo("application/pdf"),
                                  eqTo(fileBytes.length.toLong)))
      .thenAnswer((i: InvocationOnMock) => {
        val fn = i.getArgument[String](1)
        Success(s"resource/$fn")
      })

    val uploaded = service.uploadFile(fileToUpload)

    val storageKeyCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    uploaded.isSuccess should be(true)
    verify(fileStorage, times(1)).resourceExists(anyString)
    verify(fileStorage, times(1)).uploadResourceFromStream(any[ByteArrayInputStream],
                                                           storageKeyCaptor.capture(),
                                                           eqTo("application/pdf"),
                                                           eqTo(fileBytes.length.toLong))
    storageKeyCaptor.getValue.endsWith(".pdf") should be(true)
  }

  test("That updateStatus indexes the updated article") {
    reset(articleIndexService)
    reset(searchApiClient)

    val articleToUpdate = TestData.sampleDomainArticle.copy(id = Some(10), updated = yesterday)
    val user = UserInfo("Pelle", Set(Role.WRITE))
    val updatedArticle = converterService
      .updateStatus(ArticleStatus.PROPOSAL, articleToUpdate, user, isImported = false)
      .attempt
      .unsafeRunSync()
      .toTry
      .get
      .get
    val updatedAndInserted = updatedArticle
      .copy(revision = updatedArticle.revision.map(_ + 1),
            updated = today,
            notes = updatedArticle.notes.map(_.copy(timestamp = today)))

    when(draftRepository.withId(10)).thenReturn(Some(articleToUpdate))
    when(draftRepository.updateArticle(any[Article], eqTo(false))).thenReturn(Success(updatedAndInserted))

    when(articleIndexService.indexDocument(any[Article])).thenReturn(Success(updatedAndInserted))
    when(searchApiClient.indexDraft(any[Article])).thenReturn(updatedAndInserted)

    service.updateArticleStatus(ArticleStatus.PROPOSAL, 10, user, isImported = false)

    val argCap1: ArgumentCaptor[Article] = ArgumentCaptor.forClass(classOf[Article])
    val argCap2: ArgumentCaptor[Article] = ArgumentCaptor.forClass(classOf[Article])

    verify(articleIndexService, times(1)).indexDocument(argCap1.capture())
    verify(searchApiClient, times(1)).indexDraft(argCap2.capture())

    val captured1 = argCap1.getValue
    captured1.copy(updated = today, notes = captured1.notes.map(_.copy(timestamp = today))) should be(
      updatedAndInserted)

    val captured2 = argCap2.getValue
    captured2.copy(updated = today, notes = captured2.notes.map(_.copy(timestamp = today))) should be(
      updatedAndInserted)
  }
  test("That we only validate the given language") {
    val updatedArticle = TestData.sampleApiUpdateArticle.copy(language = Some("nb"))
    val article =
      TestData.sampleDomainArticle.copy(
        id = Some(5),
        content =
          Seq(ArticleContent("<section> Valid Content </section>", "nb"), ArticleContent("<div> content <div", "nn"))
      )
    val nbArticle =
      article.copy(
        content = Seq(ArticleContent("<section> Valid Content </section>", "nb"))
      )

    when(draftRepository.withId(anyLong)).thenReturn(Some(article))
    service.updateArticle(1, updatedArticle, List(), List(), TestData.userWithPublishAccess, None, None, None)

    val argCap: ArgumentCaptor[Article] = ArgumentCaptor.forClass(classOf[Article])
    verify(contentValidator, times(1)).validateArticle(argCap.capture(), any[Boolean])
    val captured = argCap.getValue
    captured.content should equal(nbArticle.content)
  }

  test("That articles are cloned with reasonable values") {
    val yesterday = new DateTime().minusDays(1)
    val today = new DateTime()

    when(clock.now()).thenReturn(today.toDate)

    withFrozenTime(today) {
      val article =
        TestData.sampleDomainArticle.copy(
          id = Some(5),
          title = Seq(domain.ArticleTitle("Tittel", "nb"), domain.ArticleTitle("Title", "en")),
          status = Status(ArticleStatus.PUBLISHED, Set(ArticleStatus.IMPORTED)),
          updated = yesterday.toDate,
          created = yesterday.minusDays(1).toDate,
          published = yesterday.toDate
        )

      val userinfo = UserInfo("somecoolid", Set.empty)

      val newId = 1231.toLong
      when(draftRepository.newEmptyArticle(any[List[String]], any[List[String]])(any[DBSession]))
        .thenReturn(Success(newId))

      val expectedInsertedArticle = article.copy(
        id = Some(newId),
        title = Seq(domain.ArticleTitle("Tittel (Kopi)", "nb"), domain.ArticleTitle("Title (Kopi)", "en")),
        revision = Some(1),
        updated = today.toDate,
        created = today.toDate,
        published = today.toDate,
        updatedBy = userinfo.id,
        status = Status(ArticleStatus.DRAFT, Set.empty),
        notes = article.notes ++
          converterService
            .newNotes(Seq("Opprettet artikkel, som kopi av artikkel med id: '5'."),
                      userinfo,
                      Status(ArticleStatus.DRAFT, Set.empty))
            .get
      )
      when(draftRepository.withId(anyLong)).thenReturn(Some(article))
      when(draftRepository.insert(any[Article])(any[DBSession])).thenAnswer((i: InvocationOnMock) =>
        i.getArgument[Article](0))

      service.copyArticleFromId(5, userinfo, "all", true, true)

      val cap: ArgumentCaptor[Article] = ArgumentCaptor.forClass(classOf[Article])
      verify(draftRepository, times(1)).insert(cap.capture())(any[DBSession])
      val insertedArticle = cap.getValue
      insertedArticle should be(expectedInsertedArticle)
    }
  }

  test("That articles are cloned without title postfix if flag is false") {
    val yesterday = new DateTime().minusDays(1)
    val today = new DateTime()

    when(clock.now()).thenReturn(today.toDate)

    withFrozenTime(today) {
      val article =
        TestData.sampleDomainArticle.copy(
          id = Some(5),
          title = Seq(domain.ArticleTitle("Tittel", "nb"), domain.ArticleTitle("Title", "en")),
          status = Status(ArticleStatus.PUBLISHED, Set(ArticleStatus.IMPORTED)),
          updated = yesterday.toDate,
          created = yesterday.minusDays(1).toDate,
          published = yesterday.toDate
        )

      val userinfo = UserInfo("somecoolid", Set.empty)

      val newId = 1231.toLong
      when(draftRepository.newEmptyArticle(any[List[String]], any[List[String]])(any[DBSession]))
        .thenReturn(Success(newId))

      val expectedInsertedArticle = article.copy(
        id = Some(newId),
        revision = Some(1),
        updated = today.toDate,
        created = today.toDate,
        published = today.toDate,
        updatedBy = userinfo.id,
        status = Status(ArticleStatus.DRAFT, Set.empty),
        notes = article.notes ++
          converterService
            .newNotes(Seq("Opprettet artikkel, som kopi av artikkel med id: '5'."),
                      userinfo,
                      Status(ArticleStatus.DRAFT, Set.empty))
            .get
      )
      when(draftRepository.withId(anyLong)).thenReturn(Some(article))
      when(draftRepository.insert(any[Article])(any[DBSession])).thenAnswer((i: InvocationOnMock) =>
        i.getArgument[Article](0))

      service.copyArticleFromId(5, userinfo, "all", true, false)

      val cap: ArgumentCaptor[Article] = ArgumentCaptor.forClass(classOf[Article])
      verify(draftRepository, times(1)).insert(cap.capture())(any[DBSession])
      val insertedArticle = cap.getValue
      insertedArticle should be(expectedInsertedArticle)
    }
  }

  test("article status should not be updated if only notes are changed") {
    val updatedArticle = TestData.blankUpdatedArticle.copy(
      revision = 1,
      notes = Some(Seq("note1", "note2")),
      editorLabels = Some(Seq("note3", "note4"))
    )

    val existing = TestData.sampleDomainArticle.copy(status = TestData.statusWithPublished)
    when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
    val Success(result1) = service.updateArticle(existing.id.get,
                                                 updatedArticle,
                                                 List.empty,
                                                 Seq.empty,
                                                 TestData.userWithWriteAccess,
                                                 None,
                                                 None,
                                                 None)
    result1.status.current should be(existing.status.current.toString)
    result1.status.other.sorted should be(existing.status.other.map(_.toString).toSeq.sorted)
  }

  test("article status should not be updated if changes only affect notes") {
    val existingTitle = "apekatter"
    val updatedArticle = TestData.blankUpdatedArticle.copy(
      revision = 1,
      language = Some("nb"),
      title = Some(existingTitle),
      notes = Some(Seq("note1", "note2")),
      editorLabels = Some(Seq("note3", "note4"))
    )

    val existing = TestData.sampleDomainArticle.copy(title = Seq(ArticleTitle(existingTitle, "nb")),
                                                     status = TestData.statusWithPublished)
    when(draftRepository.withId(existing.id.get)).thenReturn(Some(existing))
    val Success(result1) = service.updateArticle(existing.id.get,
                                                 updatedArticle,
                                                 List.empty,
                                                 Seq.empty,
                                                 TestData.userWithWriteAccess,
                                                 None,
                                                 None,
                                                 None)
    result1.status.current should be(existing.status.current.toString)
    result1.status.other.sorted should be(existing.status.other.map(_.toString).toSeq.sorted)
  }

  test("Deleting storage should be called with correct path") {
    val imported = "https://api.ndla.no/files/194277/Temahefte%20egg%20og%20meieriprodukterNN.pdf"
    val notImported = "https://api.ndla.no/files/resources/01f6TKKF1wpAsc1Z.pdf"
    val onlyPath = "resources/01f6TKKF1wpAsc1Z.pdf"
    val pathWithSlash = "/resources/01f6TKKF1wpAsc1Z.pdf"
    val pathWithFiles = "/files/resources/01f6TKKF1wpAsc1Z.pdf"
    service.getFilePathFromUrl(imported) should be("194277/Temahefte egg og meieriprodukterNN.pdf")
    service.getFilePathFromUrl(notImported) should be("resources/01f6TKKF1wpAsc1Z.pdf")
    service.getFilePathFromUrl(onlyPath) should be("resources/01f6TKKF1wpAsc1Z.pdf")
    service.getFilePathFromUrl(pathWithSlash) should be("resources/01f6TKKF1wpAsc1Z.pdf")
    service.getFilePathFromUrl(pathWithFiles) should be("resources/01f6TKKF1wpAsc1Z.pdf")
  }

  test("Article should not be saved, but only copied if createNewVersion is specified") {
    val updatedArticle = TestData.blankUpdatedArticle.copy(
      language = Some("nb"),
      title = Some("detteErEnNyTittel"),
      createNewVersion = Some(true)
    )

    service.updateArticle(articleId,
                          updatedArticle,
                          List.empty,
                          Seq.empty,
                          TestData.userWithAdminAccess,
                          None,
                          None,
                          None)

    verify(draftRepository, never).updateArticle(any[Article], anyBoolean)(any[DBSession])
    verify(draftRepository, times(1)).storeArticleAsNewVersion(any[Article])(any[DBSession])
  }

  test("contentWithClonedFiles clones files as expected") {
    when(fileStorage.copyResource(anyString(), anyString()))
      .thenReturn(
        Success("resources/new123.pdf"),
        Success("resources/new456.pdf"),
        Success("resources/new789.pdf"),
        Success("resources/new101112.pdf"),
      )
    val embed1 =
      """<embed data-alt="Kul alt1" data-path="/files/resources/abc123.pdf" data-resource="file" data-title="Kul tittel1" data-type="pdf">"""
    val embed2 =
      """<embed data-alt="Kul alt2" data-path="/files/resources/abc456.pdf" data-resource="file" data-title="Kul tittel2" data-type="pdf">"""
    val embed3 =
      """<embed data-alt="Kul alt3" data-path="/files/resources/abc789.pdf" data-resource="file" data-title="Kul tittel3" data-type="pdf">"""
    val contentNb = domain.ArticleContent(s"<section><h1>Hei</h1>$embed1$embed2</section>", "nb")
    val contentEn = domain.ArticleContent(s"<section><h1>Hello</h1>$embed1$embed3</section>", "en")

    val expectedEmbed1 =
      """<embed data-alt="Kul alt1" data-path="/files/resources/new123.pdf" data-resource="file" data-title="Kul tittel1" data-type="pdf">"""
    val expectedEmbed2 =
      """<embed data-alt="Kul alt2" data-path="/files/resources/new456.pdf" data-resource="file" data-title="Kul tittel2" data-type="pdf">"""
    val expectedEmbed3 =
      """<embed data-alt="Kul alt1" data-path="/files/resources/new789.pdf" data-resource="file" data-title="Kul tittel1" data-type="pdf">"""
    val expectedEmbed4 =
      """<embed data-alt="Kul alt3" data-path="/files/resources/new101112.pdf" data-resource="file" data-title="Kul tittel3" data-type="pdf">"""

    val expectedNb = domain.ArticleContent(s"<section><h1>Hei</h1>$expectedEmbed1$expectedEmbed2</section>", "nb")
    val expectedEn = domain.ArticleContent(s"<section><h1>Hello</h1>$expectedEmbed3$expectedEmbed4</section>", "en")

    val result = service.contentWithClonedFiles(List(contentNb, contentEn))

    result should be(Success(List(expectedNb, expectedEn)))

  }

  test("cloneEmbedAndUpdateElement updates file embeds") {
    import scala.jdk.CollectionConverters._
    val embed1 =
      """<embed data-alt="Kul alt1" data-path="/files/resources/abc123.pdf" data-resource="file" data-title="Kul tittel1" data-type="pdf">"""
    val embed2 =
      """<embed data-alt="Kul alt2" data-path="/files/resources/abc456.pdf" data-resource="file" data-title="Kul tittel2" data-type="pdf">"""

    val doc = HtmlTagRules.stringToJsoupDocument(s"<section>$embed1</section><section>$embed2</section>")
    val embeds = doc.select(s"embed[data-resource='file']").asScala
    when(fileStorage.copyResource(anyString, anyString)).thenReturn(
      Success("resources/new123.pdf"),
      Success("resources/new456.pdf")
    )

    val results = embeds.map(service.cloneEmbedAndUpdateElement)
    results.map(_.isSuccess should be(true))

    val expectedEmbed1 =
      """<embed data-alt="Kul alt1" data-path="/files/resources/new123.pdf" data-resource="file" data-title="Kul tittel1" data-type="pdf">"""
    val expectedEmbed2 =
      """<embed data-alt="Kul alt2" data-path="/files/resources/new456.pdf" data-resource="file" data-title="Kul tittel2" data-type="pdf">"""

    HtmlTagRules.jsoupDocumentToString(doc) should be(
      s"<section>$expectedEmbed1</section><section>$expectedEmbed2</section>")
  }

  test("That partialArticleFieldsUpdate updates fields correctly based on language") {
    val existingArticle = TestData.sampleDomainArticle.copy(
      availability = Availability.everyone,
      grepCodes = Seq("A", "B"),
      copyright = Some(Copyright(Some("CC-BY-4.0"), Some("origin"), Seq(), Seq(), Seq(), None, None, None)),
      metaDescription = Seq(
        ArticleMetaDescription("oldDesc", "nb"),
        ArticleMetaDescription("oldDescc", "es"),
        ArticleMetaDescription("oldDesccc", "ru"),
        ArticleMetaDescription("oldDescccc", "nn")
      ),
      tags = Seq(ArticleTag(Seq("old", "tag"), "nb"),
                 ArticleTag(Seq("guten", "tag"), "de"),
                 ArticleTag(Seq("oldd", "tagg"), "es"))
    )

    val articleFieldsToUpdate = Seq(
      api.PartialArticleFields.availability,
      api.PartialArticleFields.grepCodes,
      api.PartialArticleFields.license,
      api.PartialArticleFields.metaDescription,
      api.PartialArticleFields.tags
    )

    val expectedPartialPublishFields = integration.PartialPublishArticle(
      availability = Some(Availability.everyone),
      grepCodes = Some(Seq("A", "B")),
      license = Some("CC-BY-4.0"),
      metaDescription = Some(Seq(ArticleMetaDescription("oldDesc", "nb"))),
      tags = Some(Seq(ArticleTag(Seq("old", "tag"), "nb")))
    )
    val expectedPartialPublishFieldsLangEN = integration.PartialPublishArticle(
      availability = Some(Availability.everyone),
      grepCodes = Some(Seq("A", "B")),
      license = Some("CC-BY-4.0"),
      metaDescription = Some(Seq.empty),
      tags = Some(Seq.empty)
    )
    val expectedPartialPublishFieldsLangALL = integration.PartialPublishArticle(
      availability = Some(Availability.everyone),
      grepCodes = Some(Seq("A", "B")),
      license = Some("CC-BY-4.0"),
      metaDescription = Some(
        Seq(
          ArticleMetaDescription("oldDesc", "nb"),
          ArticleMetaDescription("oldDescc", "es"),
          ArticleMetaDescription("oldDesccc", "ru"),
          ArticleMetaDescription("oldDescccc", "nn")
        )),
      tags = Some(
        Seq(ArticleTag(Seq("old", "tag"), "nb"),
            ArticleTag(Seq("guten", "tag"), "de"),
            ArticleTag(Seq("oldd", "tagg"), "es")))
    )

    service.partialArticleFieldsUpdate(existingArticle, articleFieldsToUpdate, "nb") should be(
      expectedPartialPublishFields)
    service.partialArticleFieldsUpdate(existingArticle, articleFieldsToUpdate, "en") should be(
      expectedPartialPublishFieldsLangEN)
    service.partialArticleFieldsUpdate(existingArticle, articleFieldsToUpdate, "all") should be(
      expectedPartialPublishFieldsLangALL)
  }

  test("That updateArticle updates relatedContent") {
    val apiRelatedContent1 = api.RelatedContentLink("url1", "title1")
    val domainRelatedContent1 = domain.RelatedContentLink("url1", "title1")
    val relatedContent2 = 2

    val updatedApiArticle = TestData.blankUpdatedArticle.copy(
      revision = 1,
      relatedContent = Some(Seq(Left(apiRelatedContent1), Right(relatedContent2)))
    )
    val expectedArticle =
      article.copy(revision = Some(article.revision.get + 1),
                   relatedContent = Seq(Left(domainRelatedContent1), Right(relatedContent2)),
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

}
