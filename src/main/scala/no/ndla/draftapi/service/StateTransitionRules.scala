/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import cats.effect.IO
import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.IllegalStatusStateTransition
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.auth.UserInfo.{AdminRoles, SetPublishRoles}
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.domain.{Article, ArticleStatus, StateTransition}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service.search.ArticleIndexService

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait StateTransitionRules {
  this: WriteService with DraftRepository with ArticleApiClient with ArticleIndexService =>

  object StateTransitionRules {
    private def publishArticle(article: domain.Article): Try[Article] = {
      val externalIds = draftRepository.getExternalIdsFromId(article.id.get)
      articleApiClient.updateArticle(article.id.get, article, externalIds)
    }

    private def unpublishArticle(article: domain.Article): Try[domain.Article] =
      articleApiClient.unpublishArticle(article)

    private def removeFromSearch(article: domain.Article): Try[domain.Article] =
      articleIndexService.deleteDocument(article.id.get).map(_ => article)

    import StateTransition._

    val StateTransitions: Set[StateTransition] = Set(
      IMPORTED -> DRAFT,
      (DRAFT -> DRAFT) discardCurrentOnTransition,
      (DRAFT -> PROPOSAL) discardCurrentOnTransition,
      (DRAFT -> PUBLISHED) require AdminRoles withSideEffect publishArticle discardCurrentOnTransition,
      PROPOSAL -> USER_TEST,
      (PROPOSAL -> QUEUED_FOR_PUBLISHING) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require SetPublishRoles discardCurrentOnTransition,
      (PROPOSAL -> PUBLISHED) require AdminRoles withSideEffect publishArticle discardCurrentOnTransition,
      PROPOSAL -> AWAITING_QUALITY_ASSURANCE,
      (USER_TEST -> PROPOSAL) discardCurrentOnTransition,
      (USER_TEST -> AWAITING_QUALITY_ASSURANCE) keepStates Set(IMPORTED, PROPOSAL),
      (USER_TEST -> PUBLISHED) require AdminRoles withSideEffect publishArticle,
      (AWAITING_QUALITY_ASSURANCE -> USER_TEST) keepStates Set(IMPORTED, PROPOSAL) discardCurrentOnTransition,
      (AWAITING_QUALITY_ASSURANCE -> QUALITY_ASSURED) keepStates Set(IMPORTED, USER_TEST) discardCurrentOnTransition,
      (AWAITING_QUALITY_ASSURANCE -> PUBLISHED) require AdminRoles keepStates Set(IMPORTED, USER_TEST) withSideEffect publishArticle discardCurrentOnTransition,
      (QUALITY_ASSURED -> QUEUED_FOR_PUBLISHING) keepStates Set(IMPORTED, USER_TEST) require SetPublishRoles,
      (QUALITY_ASSURED -> PUBLISHED) require AdminRoles keepStates Set(IMPORTED, USER_TEST) withSideEffect publishArticle,
      (QUEUED_FOR_PUBLISHING -> PUBLISHED) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle discardCurrentOnTransition,
      (QUEUED_FOR_PUBLISHING -> DRAFT) discardCurrentOnTransition,
      (PUBLISHED -> DRAFT) discardCurrentOnTransition,
      (PUBLISHED -> AWAITING_UNPUBLISHING) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
      (PUBLISHED -> UNPUBLISHED) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect unpublishArticle discardCurrentOnTransition,
      (AWAITING_UNPUBLISHING -> DRAFT) discardCurrentOnTransition,
      (AWAITING_UNPUBLISHING -> PUBLISHED) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle discardCurrentOnTransition,
      (AWAITING_UNPUBLISHING -> UNPUBLISHED) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect unpublishArticle discardCurrentOnTransition,
      (UNPUBLISHED -> PUBLISHED) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle discardCurrentOnTransition,
      (UNPUBLISHED -> DRAFT) discardCurrentOnTransition,
      (UNPUBLISHED -> ARCHIVED) keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect removeFromSearch discardCurrentOnTransition
    )

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
