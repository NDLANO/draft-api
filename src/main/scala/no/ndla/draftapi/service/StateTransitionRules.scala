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

    val StateTransitions = Set(
      StateTransition(IMPORTED, DRAFT),
      StateTransition(DRAFT, DRAFT, addCurrentStateToOthersOnTransition = false),
      StateTransition(DRAFT, PROPOSAL, addCurrentStateToOthersOnTransition = false),
      StateTransition(DRAFT,
                      PUBLISHED,
                      addCurrentStateToOthersOnTransition = false,
                      sideEffect = publishArticle,
                      requiredRoles = AdminRoles),
      StateTransition(PROPOSAL, USER_TEST),
      StateTransition(PROPOSAL,
                      QUEUED_FOR_PUBLISHING,
                      Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
                      addCurrentStateToOthersOnTransition = false,
                      requiredRoles = SetPublishRoles),
      StateTransition(PROPOSAL,
                      PUBLISHED,
                      addCurrentStateToOthersOnTransition = false,
                      sideEffect = publishArticle,
                      requiredRoles = AdminRoles),
      StateTransition(PROPOSAL, AWAITING_QUALITY_ASSURANCE),
      StateTransition(USER_TEST, PROPOSAL, addCurrentStateToOthersOnTransition = false),
      StateTransition(USER_TEST, AWAITING_QUALITY_ASSURANCE, Set(IMPORTED, PROPOSAL)),
      StateTransition(USER_TEST, PUBLISHED, sideEffect = publishArticle, requiredRoles = AdminRoles),
      StateTransition(AWAITING_QUALITY_ASSURANCE,
                      USER_TEST,
                      Set(IMPORTED, PROPOSAL),
                      addCurrentStateToOthersOnTransition = false),
      StateTransition(AWAITING_QUALITY_ASSURANCE,
                      QUALITY_ASSURED,
                      Set(IMPORTED, USER_TEST),
                      addCurrentStateToOthersOnTransition = false),
      StateTransition(
        AWAITING_QUALITY_ASSURANCE,
        PUBLISHED,
        Set(IMPORTED, USER_TEST),
        addCurrentStateToOthersOnTransition = false,
        sideEffect = publishArticle,
        requiredRoles = AdminRoles
      ),
      StateTransition(QUALITY_ASSURED,
                      QUEUED_FOR_PUBLISHING,
                      Set(IMPORTED, USER_TEST),
                      requiredRoles = SetPublishRoles),
      StateTransition(QUALITY_ASSURED,
                      PUBLISHED,
                      Set(IMPORTED, USER_TEST),
                      sideEffect = publishArticle,
                      requiredRoles = AdminRoles),
      StateTransition(
        QUEUED_FOR_PUBLISHING,
        PUBLISHED,
        Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
        sideEffect = publishArticle,
        addCurrentStateToOthersOnTransition = false,
        requiredRoles = AdminRoles
      ),
      StateTransition(QUEUED_FOR_PUBLISHING, DRAFT, addCurrentStateToOthersOnTransition = false),
      StateTransition(PUBLISHED, DRAFT, addCurrentStateToOthersOnTransition = false),
      StateTransition(PUBLISHED, AWAITING_UNPUBLISHING, Set(IMPORTED, USER_TEST, QUALITY_ASSURED)),
      StateTransition(
        PUBLISHED,
        UNPUBLISHED,
        Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
        addCurrentStateToOthersOnTransition = false,
        sideEffect = unpublishArticle,
        requiredRoles = AdminRoles
      ),
      StateTransition(AWAITING_UNPUBLISHING, DRAFT, addCurrentStateToOthersOnTransition = false),
      StateTransition(
        AWAITING_UNPUBLISHING,
        PUBLISHED,
        Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
        addCurrentStateToOthersOnTransition = false,
        sideEffect = publishArticle,
        requiredRoles = AdminRoles
      ),
      StateTransition(
        AWAITING_UNPUBLISHING,
        UNPUBLISHED,
        Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
        addCurrentStateToOthersOnTransition = false,
        sideEffect = unpublishArticle,
        requiredRoles = AdminRoles
      ),
      StateTransition(
        UNPUBLISHED,
        PUBLISHED,
        Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
        addCurrentStateToOthersOnTransition = false,
        sideEffect = publishArticle,
        requiredRoles = AdminRoles
      ),
      StateTransition(UNPUBLISHED, DRAFT, addCurrentStateToOthersOnTransition = false),
      StateTransition(
        UNPUBLISHED,
        ARCHIEVED,
        Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
        addCurrentStateToOthersOnTransition = false,
        sideEffect = removeFromSearch,
        requiredRoles = AdminRoles
      )
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
