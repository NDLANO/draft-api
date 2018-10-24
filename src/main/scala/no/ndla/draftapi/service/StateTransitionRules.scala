/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import cats.effect.IO
import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.{IllegalStatusStateTransition, NotFoundException}
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.auth.UserInfo.{AdminRoles, SetPublishRoles}
import no.ndla.draftapi.integration.{ArticleApiClient, TaxonomyApiClient}
import no.ndla.draftapi.model.domain.{Article, ArticleStatus, StateTransition}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service.search.ArticleIndexService

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait StateTransitionRules {
  this: WriteService with DraftRepository with ArticleApiClient with TaxonomyApiClient with ArticleIndexService =>

  object StateTransitionRules {
    private def publishArticle(article: domain.Article): Try[Article] = {
      article.id match {
        case Some(id) =>
          val externalIds = draftRepository.getExternalIdsFromId(id)
          taxonomyApiClient.updateTaxonomyIfExists(id, article)
          articleApiClient.updateArticle(id, article, externalIds)
        case _ => Failure(NotFoundException("This is a bug, article to publish has no id."))
      }
    }

    private def unpublishArticle(article: domain.Article): Try[domain.Article] =
      // TODO: Maybe remove taxonomy?
      articleApiClient.unpublishArticle(article)

    private def removeFromSearch(article: domain.Article): Try[domain.Article] =
      // TODO: Maybe remove taxonomy?
      articleIndexService.deleteDocument(article.id.get).map(_ => article)

    import StateTransition._

    // format: off
    val StateTransitions: Set[StateTransition] = Set(
      (IMPORTED                   -> DRAFT)                      keepCurrentOnTransition,
       DRAFT                      -> DRAFT,
       DRAFT                      -> PROPOSAL,
      (DRAFT                      -> PUBLISHED)                  require AdminRoles withSideEffect publishArticle,
       PROPOSAL                   -> PROPOSAL,
       PROPOSAL                   -> DRAFT,
      (PROPOSAL                   -> USER_TEST)                  keepCurrentOnTransition,
      (PROPOSAL                   -> QUEUED_FOR_PUBLISHING)      keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require SetPublishRoles,
      (PROPOSAL                   -> PUBLISHED)                  require AdminRoles withSideEffect publishArticle,
      (PROPOSAL                   -> AWAITING_QUALITY_ASSURANCE) keepCurrentOnTransition,
      (USER_TEST                  -> USER_TEST)                  keepStates Set(IMPORTED, PROPOSAL),
       USER_TEST                  -> PROPOSAL,
       USER_TEST                  -> DRAFT,
      (USER_TEST                  -> AWAITING_QUALITY_ASSURANCE) keepStates Set(IMPORTED, PROPOSAL) keepCurrentOnTransition,
      (USER_TEST                  -> PUBLISHED)                  require AdminRoles withSideEffect publishArticle keepCurrentOnTransition,
      (AWAITING_QUALITY_ASSURANCE -> AWAITING_QUALITY_ASSURANCE) keepStates Set(IMPORTED, PROPOSAL, USER_TEST),
       AWAITING_QUALITY_ASSURANCE -> DRAFT,
      (AWAITING_QUALITY_ASSURANCE -> USER_TEST)                  keepStates Set(IMPORTED, PROPOSAL),
      (AWAITING_QUALITY_ASSURANCE -> QUALITY_ASSURED)            keepStates Set(IMPORTED, USER_TEST),
      (AWAITING_QUALITY_ASSURANCE -> PUBLISHED)                  require AdminRoles keepStates Set(IMPORTED, USER_TEST) withSideEffect publishArticle,
       QUALITY_ASSURED            -> DRAFT,
       QUALITY_ASSURED            -> QUALITY_ASSURED,
      (QUALITY_ASSURED            -> QUEUED_FOR_PUBLISHING)      keepStates Set(IMPORTED, USER_TEST) require SetPublishRoles keepCurrentOnTransition,
      (QUALITY_ASSURED            -> PUBLISHED)                  require AdminRoles keepStates Set(IMPORTED, USER_TEST) withSideEffect publishArticle keepCurrentOnTransition,
      (QUEUED_FOR_PUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle,
       QUEUED_FOR_PUBLISHING      -> QUEUED_FOR_PUBLISHING,
       QUEUED_FOR_PUBLISHING      -> DRAFT,
       PUBLISHED                  -> DRAFT,
       PUBLISHED                  -> PROPOSAL,
      (PUBLISHED                  -> AWAITING_UNPUBLISHING)      keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) keepCurrentOnTransition,
      (PUBLISHED                  -> UNPUBLISHED)                keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect unpublishArticle,
       AWAITING_UNPUBLISHING      -> DRAFT,
       AWAITING_UNPUBLISHING      -> AWAITING_UNPUBLISHING,
      (AWAITING_UNPUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle,
      (AWAITING_UNPUBLISHING      -> UNPUBLISHED)                keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect unpublishArticle,
      (UNPUBLISHED                -> PUBLISHED)                  keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle,
       UNPUBLISHED                -> PROPOSAL,
       UNPUBLISHED                -> DRAFT,
      (UNPUBLISHED                -> ARCHIVED)                   keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect removeFromSearch
    )
    // format: on

    private def getTransition(from: ArticleStatus.Value,
                              to: ArticleStatus.Value,
                              user: UserInfo): Option[StateTransition] = {
      StateTransitions.find(transition => transition.from == from && transition.to == to) match {
        case Some(t) if user.hasRoles(t.requiredRoles) || user.isAdmin => Some(t)
        case _                                                         => None
      }
    }

    private[service] def doTransitionWithoutSideEffect(
        current: domain.Article,
        to: ArticleStatus.Value,
        user: UserInfo): (Try[domain.Article], domain.Article => Try[domain.Article]) = {
      getTransition(current.status.current, to, user) match {
        case Some(t) =>
          val currentToOther = if (t.addCurrentStateToOthersOnTransition) Set(current.status.current) else Set()
          val other = current.status.other.intersect(t.otherStatesToKeepOnTransition) ++ currentToOther

          val convertedArticle = current.copy(status = domain.Status(to, other))
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

  }

}
