/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.util.Date

import cats.effect.IO
import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.{IllegalStatusStateTransition, NotFoundException}
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.auth.UserInfo.{PublishRoles, DirectPublishRoles}
import no.ndla.draftapi.integration.{ArticleApiClient, LearningPath, LearningpathApiClient, TaxonomyApiClient}
import no.ndla.draftapi.model.domain.{ArticleStatus, StateTransition}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service.SideEffect.SideEffect
import no.ndla.draftapi.service.search.ArticleIndexService
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.validation.{ValidationException, ValidationMessage}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait StateTransitionRules {
  this: WriteService
    with DraftRepository
    with ArticleApiClient
    with TaxonomyApiClient
    with LearningpathApiClient
    with ConverterService
    with ContentValidator
    with ArticleIndexService =>

  object StateTransitionRules {

    // Import implicits to clean up SideEffect creation where we don't need all parameters
    import SideEffect.implicits._

    private[service] val checkIfArticleIsUsedInLearningStep: SideEffect = (article: domain.Article) =>
      doIfArticleIsUnusedByLearningpath(article.id.getOrElse(1)) {
        Success(article)
    }

    private[service] val unpublishArticle: SideEffect = (article: domain.Article) =>
      doIfArticleIsUnusedByLearningpath(article.id.getOrElse(1)) {
        article.id match {
          case Some(id) =>
            taxonomyApiClient.updateTaxonomyMetadataIfExists(id, article.grepCodes, false)
            articleApiClient.unpublishArticle(article)
          case _ => Failure(NotFoundException("This is a bug, article to unpublish has no id."))
        }
    }

    private val validateArticleApiArticle: SideEffect = (article: domain.Article, isImported: Boolean) => {
      val articleApiArticle = converterService.toArticleApiArticle(article)
      articleApiClient.validateArticle(articleApiArticle, isImported).map(_ => article)
    }

    private def publishArticleSideEffect(useSoftValidation: Boolean = false): SideEffect =
      (article, isImported) =>
        article.id match {
          case Some(id) =>
            val externalIds = draftRepository.getExternalIdsFromId(id)
            taxonomyApiClient.updateTaxonomyIfExists(id, article)
            taxonomyApiClient.updateTaxonomyMetadataIfExists(id, article.grepCodes, true)
            articleApiClient.updateArticle(id, article, externalIds, isImported, useSoftValidation)
          case _ => Failure(NotFoundException("This is a bug, article to publish has no id."))
      }

    private val publishArticle = publishArticleSideEffect(false)
    private val publishWithSoftValidation = publishArticleSideEffect(true)

    import StateTransition._

    // format: off
    val StateTransitions: Set[StateTransition] = Set(
      (IMPORTED                   -> DRAFT)                      keepCurrentOnTransition,
       DRAFT                      -> DRAFT,
       DRAFT                      -> PROPOSAL,
       DRAFT                      -> ARCHIVED                    require PublishRoles illegalStatuses Set(PUBLISHED),
       ARCHIVED                   -> ARCHIVED,
       ARCHIVED                   -> DRAFT,
      (DRAFT                      -> PUBLISHED)                  keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect publishWithSoftValidation,
       PROPOSAL                   -> PROPOSAL,
       PROPOSAL                   -> DRAFT,
       PROPOSAL                   -> ARCHIVED                    require PublishRoles illegalStatuses Set(PUBLISHED),
       PROPOSAL                   -> QUEUED_FOR_LANGUAGE,
      (PROPOSAL                   -> USER_TEST)                  keepCurrentOnTransition,
      (PROPOSAL                   -> QUEUED_FOR_PUBLISHING)      keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED, PUBLISHED) withSideEffect validateArticleApiArticle require PublishRoles,
      (PROPOSAL                   -> PUBLISHED)                  keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect publishWithSoftValidation,
      (PROPOSAL                   -> AWAITING_QUALITY_ASSURANCE) keepCurrentOnTransition,
      (USER_TEST                  -> USER_TEST)                  keepStates Set(IMPORTED, PROPOSAL, PUBLISHED),
       USER_TEST                  -> PROPOSAL,
       USER_TEST                  -> DRAFT,
      (USER_TEST                  -> AWAITING_QUALITY_ASSURANCE) keepStates Set(IMPORTED, PROPOSAL, PUBLISHED) keepCurrentOnTransition,
      (USER_TEST                  -> PUBLISHED)                  keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect publishWithSoftValidation,
      (AWAITING_QUALITY_ASSURANCE -> AWAITING_QUALITY_ASSURANCE) keepStates Set(IMPORTED, PROPOSAL, USER_TEST, PUBLISHED),
       AWAITING_QUALITY_ASSURANCE -> DRAFT,
       AWAITING_QUALITY_ASSURANCE -> QUEUED_FOR_LANGUAGE,
      (AWAITING_QUALITY_ASSURANCE -> USER_TEST)                  keepStates Set(IMPORTED, PROPOSAL, PUBLISHED),
      (AWAITING_QUALITY_ASSURANCE -> QUALITY_ASSURED)            keepStates Set(IMPORTED, USER_TEST, PUBLISHED) withSideEffect validateArticleApiArticle,
      (AWAITING_QUALITY_ASSURANCE -> PUBLISHED)                  keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect publishWithSoftValidation,
       QUALITY_ASSURED            -> QUALITY_ASSURED,
       QUALITY_ASSURED            -> DRAFT,
      (QUALITY_ASSURED            -> QUEUED_FOR_PUBLISHING)      keepStates Set(IMPORTED, USER_TEST, PUBLISHED) require PublishRoles withSideEffect validateArticleApiArticle keepCurrentOnTransition,
      (QUALITY_ASSURED            -> PUBLISHED)                  keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect publishArticle,
       QUEUED_FOR_PUBLISHING      -> QUEUED_FOR_PUBLISHING       withSideEffect validateArticleApiArticle,
      (QUEUED_FOR_PUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect publishArticle,
       QUEUED_FOR_PUBLISHING      -> DRAFT,
      (PUBLISHED                  -> DRAFT)                      keepCurrentOnTransition,
      (PUBLISHED                  -> PROPOSAL)                   keepCurrentOnTransition,
      (PUBLISHED                  -> AWAITING_UNPUBLISHING)      require PublishRoles withSideEffect checkIfArticleIsUsedInLearningStep keepCurrentOnTransition,
      (PUBLISHED                  -> UNPUBLISHED)                keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect checkIfArticleIsUsedInLearningStep withSideEffect unpublishArticle,
      (AWAITING_UNPUBLISHING      -> AWAITING_UNPUBLISHING)      withSideEffect checkIfArticleIsUsedInLearningStep keepCurrentOnTransition,
       AWAITING_UNPUBLISHING      -> DRAFT,
      (AWAITING_UNPUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED) require PublishRoles withSideEffect publishArticle,
      (AWAITING_UNPUBLISHING      -> UNPUBLISHED)                keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect unpublishArticle,
      (UNPUBLISHED                -> PUBLISHED)                  keepStates Set(IMPORTED) require DirectPublishRoles withSideEffect publishWithSoftValidation,
       UNPUBLISHED                -> PROPOSAL,
       UNPUBLISHED                -> DRAFT,
       UNPUBLISHED                -> UNPUBLISHED,
       UNPUBLISHED                -> ARCHIVED                    require PublishRoles illegalStatuses Set(PUBLISHED),
       QUEUED_FOR_LANGUAGE        -> QUEUED_FOR_LANGUAGE,
       QUEUED_FOR_LANGUAGE        -> PROPOSAL,
       QUEUED_FOR_LANGUAGE        -> TRANSLATED,
       TRANSLATED                 -> TRANSLATED,
       TRANSLATED                 -> PROPOSAL,
       TRANSLATED                 -> AWAITING_QUALITY_ASSURANCE
    )
    // format: on

    private def getTransition(from: ArticleStatus.Value,
                              to: ArticleStatus.Value,
                              user: UserInfo): Option[StateTransition] = {
      StateTransitions
        .find(transition => transition.from == from && transition.to == to)
        .filter(t => user.hasRoles(t.requiredRoles))
    }

    private[service] def doTransitionWithoutSideEffect(current: domain.Article,
                                                       to: ArticleStatus.Value,
                                                       user: UserInfo,
                                                       isImported: Boolean): (Try[domain.Article], SideEffect) = {
      getTransition(current.status.current, to, user) match {
        case Some(t) =>
          val currentToOther = if (t.addCurrentStateToOthersOnTransition) Set(current.status.current) else Set()
          val containsIllegalStatuses = current.status.other.intersect(t.illegalStatuses)
          if (containsIllegalStatuses.nonEmpty) {
            val illegalStateTransition = IllegalStatusStateTransition(
              s"Cannot go to $to when article contains $containsIllegalStatuses")
            return (Failure(illegalStateTransition), SideEffect.fromOutput(Failure(illegalStateTransition)))
          }
          val other = current.status.other.intersect(t.otherStatesToKeepOnTransition) ++ currentToOther
          val newStatus = domain.Status(to, other)
          val newEditorNotes =
            if (current.status.current != to)
              current.notes :+ domain.EditorNote("Status endret",
                                                 if (isImported) "System" else user.id,
                                                 newStatus,
                                                 new Date())
            else current.notes
          val convertedArticle = current.copy(status = newStatus, notes = newEditorNotes)
          (Success(convertedArticle), t.sideEffect)
        case None =>
          val illegalStateTransition = IllegalStatusStateTransition(
            s"Cannot go to $to when article is ${current.status.current}")
          (Failure(illegalStateTransition), SideEffect.fromOutput(Failure(illegalStateTransition)))
      }
    }

    def doTransition(current: domain.Article,
                     to: ArticleStatus.Value,
                     user: UserInfo,
                     isImported: Boolean): IO[Try[domain.Article]] = {
      val (convertedArticle, sideEffect) = doTransitionWithoutSideEffect(current, to, user, isImported)
      IO { convertedArticle.flatMap(article => sideEffect(article, isImported)) }
    }

    private[this] def learningPathsUsingArticle(articleId: Long): Seq[LearningPath] = {
      val resources = taxonomyApiClient.queryResource(articleId).getOrElse(List.empty).flatMap(_.paths)
      val topics = taxonomyApiClient.queryTopic(articleId).getOrElse(List.empty).flatMap(_.paths)
      val paths = resources ++ topics :+ s"/article/$articleId"

      learningpathApiClient.getLearningpathsWithPaths(paths) match {
        case Success(learningpaths) => learningpaths
        case _                      => Seq.empty
      }
    }

    private def doIfArticleIsUnusedByLearningpath(articleId: Long)(
        callback: => Try[domain.Article]): Try[domain.Article] = {
      val pathsUsingArticle = learningPathsUsingArticle(articleId).map(_.id.getOrElse(-1))
      if (pathsUsingArticle.isEmpty)
        callback
      else
        Failure(new ValidationException(errors = Seq(ValidationMessage(
          "status.current",
          s"Learningpath(s) with id(s) ${pathsUsingArticle.mkString(",")} contains a learning step that uses this article"))))
    }

  }
}
