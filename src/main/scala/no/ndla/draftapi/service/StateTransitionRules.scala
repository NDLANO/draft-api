/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.util.Date

import cats.effect.IO
import io.lemonlabs.uri.Uri
import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.{IllegalStatusStateTransition, NotFoundException}
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.auth.UserInfo.{AdminRoles, SetPublishRoles}
import no.ndla.draftapi.integration.{
  ArticleApiClient,
  LearningPath,
  LearningStep,
  LearningpathApiClient,
  TaxonomyApiClient
}
import no.ndla.draftapi.model.domain.{Article, ArticleStatus, StateTransition}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.repository.DraftRepository
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

    import StateTransition._

    // format: off
    val StateTransitions: Set[StateTransition] = Set(
      (IMPORTED                   -> DRAFT)                      keepCurrentOnTransition,
       DRAFT                      -> DRAFT,
       DRAFT                      -> PROPOSAL,
      (DRAFT                      -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
       PROPOSAL                   -> PROPOSAL,
       PROPOSAL                   -> DRAFT,
       PROPOSAL                   -> QUEUED_FOR_LANGUAGE,
      (PROPOSAL                   -> USER_TEST)                  keepCurrentOnTransition,
      (PROPOSAL                   -> QUEUED_FOR_PUBLISHING)      keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) withSideEffect validateArticle require SetPublishRoles,
      (PROPOSAL                   -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
      (PROPOSAL                   -> AWAITING_QUALITY_ASSURANCE) keepCurrentOnTransition,
      (USER_TEST                  -> USER_TEST)                  keepStates Set(IMPORTED, PROPOSAL),
       USER_TEST                  -> PROPOSAL,
       USER_TEST                  -> DRAFT,
       USER_TEST                  -> QUEUED_FOR_LANGUAGE,
      (USER_TEST                  -> AWAITING_QUALITY_ASSURANCE) keepStates Set(IMPORTED, PROPOSAL) keepCurrentOnTransition,
      (USER_TEST                  -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
      (AWAITING_QUALITY_ASSURANCE -> AWAITING_QUALITY_ASSURANCE) keepStates Set(IMPORTED, PROPOSAL, USER_TEST),
       AWAITING_QUALITY_ASSURANCE -> DRAFT,
       AWAITING_QUALITY_ASSURANCE -> QUEUED_FOR_LANGUAGE,
      (AWAITING_QUALITY_ASSURANCE -> USER_TEST)                  keepStates Set(IMPORTED, PROPOSAL),
      (AWAITING_QUALITY_ASSURANCE -> QUALITY_ASSURED)            keepStates Set(IMPORTED, USER_TEST),
      (AWAITING_QUALITY_ASSURANCE -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
       QUALITY_ASSURED            -> QUALITY_ASSURED,
       QUALITY_ASSURED            -> DRAFT,
      (QUALITY_ASSURED            -> QUEUED_FOR_PUBLISHING)      keepStates Set(IMPORTED, USER_TEST) require SetPublishRoles withSideEffect validateArticle keepCurrentOnTransition,
      (QUALITY_ASSURED            -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
       QUEUED_FOR_PUBLISHING      -> QUEUED_FOR_PUBLISHING       withSideEffect validateArticle,
      (QUEUED_FOR_PUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
       QUEUED_FOR_PUBLISHING      -> DRAFT,
       PUBLISHED                  -> DRAFT,
       PUBLISHED                  -> PROPOSAL,
      (PUBLISHED                  -> AWAITING_UNPUBLISHING)      keepStates Set(IMPORTED) withSideEffect checkIfArticleIsUsedInLearningStep keepCurrentOnTransition,
      (PUBLISHED                  -> UNPUBLISHED)                keepStates Set(IMPORTED) require AdminRoles withSideEffect unpublishArticle,
      (AWAITING_UNPUBLISHING      -> AWAITING_UNPUBLISHING)      withSideEffect checkIfArticleIsUsedInLearningStep,
       AWAITING_UNPUBLISHING      -> DRAFT,
      (AWAITING_UNPUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
      (AWAITING_UNPUBLISHING      -> UNPUBLISHED)                keepStates Set(IMPORTED) require AdminRoles withSideEffect unpublishArticle,
      (UNPUBLISHED                -> PUBLISHED)                  keepStates Set(IMPORTED) require AdminRoles withSideEffect publishArticle,
       UNPUBLISHED                -> PROPOSAL,
       UNPUBLISHED                -> DRAFT,
      (UNPUBLISHED                -> ARCHIVED)                   keepStates Set(IMPORTED) require AdminRoles withSideEffect removeFromSearch,
      QUEUED_FOR_LANGUAGE         -> QUEUED_FOR_LANGUAGE,
      QUEUED_FOR_LANGUAGE         -> PROPOSAL,
      QUEUED_FOR_LANGUAGE         -> AWAITING_QUALITY_ASSURANCE,
      QUEUED_FOR_LANGUAGE         -> USER_TEST
    )
    // format: on

    private def getTransition(from: ArticleStatus.Value,
                              to: ArticleStatus.Value,
                              user: UserInfo): Option[StateTransition] = {
      StateTransitions
        .find(transition => transition.from == from && transition.to == to)
        .filter(t => user.hasRoles(t.requiredRoles) || user.isAdmin)
    }

    private[service] def doTransitionWithoutSideEffect(
        current: domain.Article,
        to: ArticleStatus.Value,
        user: UserInfo): (Try[domain.Article], domain.Article => Try[domain.Article]) = {
      getTransition(current.status.current, to, user) match {
        case Some(t) =>
          val currentToOther = if (t.addCurrentStateToOthersOnTransition) Set(current.status.current) else Set()
          val other = current.status.other.intersect(t.otherStatesToKeepOnTransition) ++ currentToOther
          val newStatus = domain.Status(to, other)
          val newEditorNotes =
            if (current.status.current != to)
              current.notes :+ domain.EditorNote("Status endret", user.id, newStatus, new Date())
            else current.notes
          val convertedArticle = current.copy(status = newStatus, notes = newEditorNotes)
          (Success(convertedArticle), t.sideEffect)
        case None =>
          val illegalStateTransition = IllegalStatusStateTransition(
            s"Cannot go to $to when article is ${current.status.current}")
          (Failure(illegalStateTransition), _ => Failure(illegalStateTransition))
      }
    }

    def doTransition(current: domain.Article, to: ArticleStatus.Value, user: UserInfo): IO[Try[domain.Article]] = {
      val (convertedArticle, sideEffect) = doTransitionWithoutSideEffect(current, to, user)
      IO { convertedArticle.flatMap(sideEffect) }
    }

    private def publishArticle(article: domain.Article): Try[Article] = {
      article.id match {
        case Some(id) =>
          val externalIds = draftRepository.getExternalIdsFromId(id)
          taxonomyApiClient.updateTaxonomyIfExists(id, article)
          articleApiClient.updateArticle(id, article, externalIds)
        case _ => Failure(NotFoundException("This is a bug, article to publish has no id."))
      }
    }

    private[this] def learningstepContainsArticleEmbed(articleId: Long, steps: LearningStep): Boolean = {
      val urls = steps.embedUrl.map(embed => Uri.parse(embed.url))
      val DirectArticleUrl = raw"""^.*/article/([0-9]+)$$""".r
      val TaxonomyUrl = raw"""^.+/(?:resource|topic):[0-9]:([0-9]+)$$""".r

      urls.exists(url => {
        url.path.toStringRaw match {
          case DirectArticleUrl(f)     => f == s"$articleId"
          case TaxonomyUrl(externalId) => draftRepository.getIdFromExternalId(externalId).contains(articleId)
          case _                       => false
        }
      })
    }

    private[this] def learningPathsUsingArticle(articleId: Long): Seq[LearningPath] = {
      learningpathApiClient.getLearningpaths() match {
        case Some(Success(learningpaths)) =>
          learningpaths.filter(learningpath =>
            learningpath.learningsteps.exists(learningstepContainsArticleEmbed(articleId, _)))
        case _ => Seq.empty
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

    private[service] def checkIfArticleIsUsedInLearningStep(article: domain.Article): Try[domain.Article] = {
      doIfArticleIsUnusedByLearningpath(article.id.getOrElse(1)) {
        Success(article)
      }
    }

    private[service] def unpublishArticle(article: domain.Article): Try[domain.Article] = {
      doIfArticleIsUnusedByLearningpath(article.id.getOrElse(1)) {
        articleApiClient.unpublishArticle(article)
      }
    }

    private def removeFromSearch(article: domain.Article): Try[domain.Article] =
      articleIndexService.deleteDocument(article.id.get).map(_ => article)

    private def validateArticle(article: domain.Article): Try[domain.Article] =
      contentValidator.validateArticle(article, false)

  }
}
