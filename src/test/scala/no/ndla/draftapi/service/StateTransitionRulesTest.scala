/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.util.Date

import no.ndla.draftapi.caching.Memoize
import no.ndla.draftapi.integration.{LearningPath, LearningStep}
import no.ndla.draftapi.model.api.IllegalStatusStateTransition
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.{Article, EditorNote, Status}
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.ValidationException
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import scalikejdbc.DBSession

import scala.util.{Failure, Success, Try}

class StateTransitionRulesTest extends UnitSuite with TestEnvironment {
  import StateTransitionRules.doTransitionWithoutSideEffect

  val DraftStatus = domain.Status(DRAFT, Set(QUALITY_ASSURED))
  val DraftWithPublishedStatus = domain.Status(DRAFT, Set(IMPORTED, PUBLISHED))
  val PublishedStatus = domain.Status(PUBLISHED, Set(IMPORTED))
  val UserTestStatus = domain.Status(USER_TEST, Set(PROPOSAL, IMPORTED))
  val AwaitingUnpublishStatus = domain.Status(AWAITING_UNPUBLISHING, Set.empty)
  val UnpublishedStatus = domain.Status(UNPUBLISHED, Set.empty)
  val ProposalStatus = domain.Status(PROPOSAL, Set.empty)
  val DraftArticle = TestData.sampleArticleWithByNcSa.copy(status = DraftStatus)
  val AwaitingUnpublishArticle = TestData.sampleArticleWithByNcSa.copy(status = AwaitingUnpublishStatus)
  val UnpublishedArticle = TestData.sampleArticleWithByNcSa.copy(status = UnpublishedStatus)

  test("doTransition should succeed when performing a legal transition") {
    val expected = domain.Status(PUBLISHED, Set.empty)
    val (Success(res), _) = doTransitionWithoutSideEffect(DraftArticle, PUBLISHED, TestData.userWIthAdminAccess)

    res.status should equal(expected)
  }

  test("doTransition should keep some states when performing a legal transition") {
    val expected = domain.Status(USER_TEST, Set(PROPOSAL, IMPORTED))
    val (Success(res), _) =
      doTransitionWithoutSideEffect(DraftArticle.copy(status = UserTestStatus), USER_TEST, TestData.userWIthAdminAccess)
    res.status should equal(expected)

    val expected2 = domain.Status(DRAFT, Set(IMPORTED, PUBLISHED))
    val (Success(res2), _) =
      doTransitionWithoutSideEffect(DraftArticle.copy(status = PublishedStatus), DRAFT, TestData.userWIthAdminAccess)
    res2.status should equal(expected2)

    val expected3 = domain.Status(DRAFT, Set(IMPORTED, PUBLISHED))
    val (Success(res3), _) =
      doTransitionWithoutSideEffect(DraftArticle.copy(status = DraftWithPublishedStatus),
                                    DRAFT,
                                    TestData.userWIthAdminAccess)
    res3.status should equal(expected3)
  }

  test("doTransition should fail when performing an illegal transition") {
    val (res, _) = doTransitionWithoutSideEffect(DraftArticle, QUALITY_ASSURED, TestData.userWIthAdminAccess)
    res.isFailure should be(true)
  }

  test("doTransition should publish the article when transitioning to PUBLISHED") {
    val expectedStatus = domain.Status(PUBLISHED, Set.empty)
    val editorNotes = Seq(EditorNote("Status endret", "unit_test", expectedStatus, new Date()))
    val expectedArticle = AwaitingUnpublishArticle.copy(status = expectedStatus, notes = editorNotes)
    when(draftRepository.getExternalIdsFromId(any[Long])(any[DBSession])).thenReturn(List("1234"))
    when(articleApiClient.updateArticle(eqTo(DraftArticle.id.get), any[Article], eqTo(List("1234"))))
      .thenReturn(Success(expectedArticle))

    val (Success(res), sideEffect) =
      doTransitionWithoutSideEffect(DraftArticle, PUBLISHED, TestData.userWIthAdminAccess)
    sideEffect(res).get.status should equal(expectedStatus)

    val captor = ArgumentCaptor.forClass(classOf[Article])
    verify(articleApiClient, times(1))
      .updateArticle(eqTo(DraftArticle.id.get), captor.capture(), eqTo(List("1234")))

    val argumentArticle: Article = captor.getValue
    val argumentArticleWithNotes = argumentArticle.copy(notes = editorNotes)
    argumentArticleWithNotes should equal(expectedArticle)
  }

  test("doTransition should unpublish the article when transitioning to UNPUBLISHED") {
    val expectedStatus = domain.Status(UNPUBLISHED, Set.empty)
    val editorNotes = Seq(EditorNote("Status endret", "unit_test", expectedStatus, new Date()))
    val expectedArticle = AwaitingUnpublishArticle.copy(status = expectedStatus, notes = editorNotes)

    when(learningpathApiClient.getLearningpaths).thenReturn(Memoize[Try[Seq[LearningPath]]](() => Success(Seq.empty)))
    when(articleApiClient.unpublishArticle(any[Article])).thenReturn(Success(expectedArticle))

    val (Success(res), sideEffect) =
      doTransitionWithoutSideEffect(AwaitingUnpublishArticle, UNPUBLISHED, TestData.userWIthAdminAccess)
    sideEffect(res).get.status should equal(expectedStatus)

    val captor = ArgumentCaptor.forClass(classOf[Article])

    verify(articleApiClient, times(1))
      .unpublishArticle(captor.capture())

    val argumentArticle: Article = captor.getValue
    val argumentArticleWithNotes = argumentArticle.copy(notes = editorNotes)
    argumentArticleWithNotes should equal(expectedArticle)
  }

  test("doTransition should remove article from search when transitioning to ARCHIEVED") {
    val expectedStatus = domain.Status(ARCHIVED, Set.empty)

    when(articleIndexService.deleteDocument(UnpublishedArticle.id.get)).thenReturn(Success(UnpublishedArticle.id.get))

    val (Success(res), sideEffect) =
      doTransitionWithoutSideEffect(UnpublishedArticle, ARCHIVED, TestData.userWIthAdminAccess)
    sideEffect(res).get.status should equal(expectedStatus)

    verify(articleIndexService, times(1))
      .deleteDocument(UnpublishedArticle.id.get)
  }

  test("user without required roles should not be permitted to perform the status transition") {
    val proposalArticle = TestData.sampleArticleWithByNcSa.copy(status = ProposalStatus)
    val (Failure(ex: IllegalStatusStateTransition), _) =
      doTransitionWithoutSideEffect(DraftArticle, PUBLISHED, TestData.userWithWriteAccess)
    ex.getMessage should equal("Cannot go to PUBLISHED when article is DRAFT")

    val (Failure(ex2: IllegalStatusStateTransition), _) =
      doTransitionWithoutSideEffect(proposalArticle, QUEUED_FOR_PUBLISHING, TestData.userWithWriteAccess)
    ex2.getMessage should equal("Cannot go to QUEUED_FOR_PUBLISHING when article is PROPOSAL")
  }

  test("PUBLISHED should be removed when transitioning to UNPUBLISHED") {
    val expected = domain.Status(UNPUBLISHED, Set())
    val publishedArticle = DraftArticle.copy(status = domain.Status(current = PUBLISHED, other = Set()))
    val (Success(res), _) = doTransitionWithoutSideEffect(publishedArticle, UNPUBLISHED, TestData.userWIthAdminAccess)

    res.status should equal(expected)
  }

  test("unpublishArticle should fail if article is used in a learningstep") {
    val articleId: Long = 7
    val article = TestData.sampleDomainArticle.copy(id = Some(articleId))
    val learningSteps: Seq[LearningStep] =
      Seq(LearningStep(Some(1), Seq(TestData.sampleEmbedUrl.copy(url = s"/article/$articleId"))))
    val learningPath = TestData.sampleLearningPath.copy(learningsteps = learningSteps)
    when(learningpathApiClient.getLearningpaths).thenReturn(Memoize[Try[Seq[LearningPath]]](() =>
      Success(Seq(learningPath))))

    val res = StateTransitionRules.unpublishArticle(article)
    res.isFailure should be(true)
  }

  test("unpublishArticle should fail if article is used in a learningstep with a taxonomy-url") {
    val articleId: Long = 7
    val externalId = "169243"
    val article = TestData.sampleDomainArticle.copy(id = Some(articleId))
    val learningSteps: Seq[LearningStep] =
      Seq(
        LearningStep(Some(1),
                     Seq(TestData.sampleEmbedUrl.copy(
                       url = s"/unknown/subjects/subject:15/topic:1:166611/topic:1:182229/resource:1:$externalId"))))
    val learningPath = TestData.sampleLearningPath.copy(learningsteps = learningSteps)
    when(learningpathApiClient.getLearningpaths).thenReturn(Memoize[Try[Seq[LearningPath]]](() =>
      Success(Seq(learningPath))))
    when(draftRepository.getIdFromExternalId(any[String])(any[DBSession])).thenReturn(Some(articleId.toLong))

    val res = StateTransitionRules.unpublishArticle(article)
    res.isFailure should be(true)
  }

  test("unpublishArticle should succeed if article is not used in a learningstep") {
    val articleId = 7
    val article = TestData.sampleDomainArticle.copy(id = Some(articleId))
    val learningSteps: Seq[LearningStep] =
      Seq(LearningStep(Some(1), Seq(TestData.sampleEmbedUrl.copy(url = s"/article/1234"))))
    val learningPath = TestData.sampleLearningPath.copy(learningsteps = learningSteps)
    when(learningpathApiClient.getLearningpaths).thenReturn(Memoize[Try[Seq[LearningPath]]](() =>
      Success(Seq(learningPath))))
    when(articleApiClient.unpublishArticle(article)).thenReturn(Success(article))

    val res = StateTransitionRules.unpublishArticle(article)
    res.isSuccess should be(true)
    verify(articleApiClient, times(1)).unpublishArticle(article)
  }

  test("checkIfArticleIsUsedInLearningStep should fail if article is used in a learningstep") {
    val articleId: Long = 7
    val article = TestData.sampleDomainArticle.copy(id = Some(articleId))
    val learningSteps: Seq[LearningStep] =
      Seq(LearningStep(Some(1), Seq(TestData.sampleEmbedUrl.copy(url = s"/article/$articleId"))))
    val learningPath = TestData.sampleLearningPath.copy(learningsteps = learningSteps)
    when(learningpathApiClient.getLearningpaths).thenReturn(Memoize[Try[Seq[LearningPath]]](() =>
      Success(Seq(learningPath))))

    val Failure(res: ValidationException) = StateTransitionRules.checkIfArticleIsUsedInLearningStep(article)
    res.errors.head.message should equal("Learningpath(s) with id(s) 1 contains a learning step that uses this article")
  }

  test("checkIfArticleIsUsedInLearningStep  should fail if article is used in a learningstep with a taxonomy-url") {
    val articleId: Long = 7
    val externalId = "169243"
    val article = TestData.sampleDomainArticle.copy(id = Some(articleId))
    val learningSteps: Seq[LearningStep] =
      Seq(
        LearningStep(Some(1),
                     Seq(TestData.sampleEmbedUrl.copy(
                       url = s"/unknown/subjects/subject:15/topic:1:166611/topic:1:182229/resource:1:$externalId"))))
    val learningPath = TestData.sampleLearningPath.copy(learningsteps = learningSteps)
    when(learningpathApiClient.getLearningpaths).thenReturn(Memoize[Try[Seq[LearningPath]]](() =>
      Success(Seq(learningPath))))
    when(draftRepository.getIdFromExternalId(any[String])(any[DBSession])).thenReturn(Some(articleId.toLong))

    val Failure(res: ValidationException) = StateTransitionRules.checkIfArticleIsUsedInLearningStep(article)
    res.errors.head.message should equal("Learningpath(s) with id(s) 1 contains a learning step that uses this article")
  }

  test("checkIfArticleIsUsedInLearningStep  should succeed if article is not used in a learningstep") {
    val articleId = 7
    val article = TestData.sampleDomainArticle.copy(id = Some(articleId))
    val learningSteps: Seq[LearningStep] =
      Seq(LearningStep(Some(1), Seq(TestData.sampleEmbedUrl.copy(url = s"/article/1234"))))
    val learningPath = TestData.sampleLearningPath.copy(learningsteps = learningSteps)
    when(learningpathApiClient.getLearningpaths).thenReturn(Memoize[Try[Seq[LearningPath]]](() =>
      Success(Seq(learningPath))))
    when(articleApiClient.unpublishArticle(article)).thenReturn(Success(article))

    val res = StateTransitionRules.checkIfArticleIsUsedInLearningStep(article)
    res.isSuccess should be(true)
    verify(articleApiClient, times(1)).unpublishArticle(article)
  }

  test("validateArticle should be called when transitioning to QUEUED_FOR_PUBLISHING") {
    val articleId = 7
    val article = TestData.sampleDomainArticle.copy(id = Some(articleId))
    val status = Status(QUALITY_ASSURED, Set.empty)

    val transitionsToTest = StateTransitionRules.StateTransitions.filter(_.to == QUEUED_FOR_PUBLISHING)
    transitionsToTest.foreach(
      t =>
        StateTransitionRules
          .doTransition(article.copy(status = status.copy(current = t.from)),
                        QUEUED_FOR_PUBLISHING,
                        TestData.userWIthAdminAccess)
          .unsafeRunSync())
    verify(contentValidator, times(transitionsToTest.size)).validateArticle(any[domain.Article], any[Boolean])
  }

}
