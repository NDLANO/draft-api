/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import cats.effect.IO
import com.netaporter.uri.Uri
import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.{IllegalStatusStateTransition, ValidationException}
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.auth.UserInfo.{AdminRoles, SetPublishRoles}
import no.ndla.draftapi.integration.{ArticleApiClient, LearningPath, LearningStep, LearningpathApiClient}
import no.ndla.draftapi.model.domain.{Article, ArticleStatus, StateTransition}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service.search.ArticleIndexService

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait StateTransitionRules {
  this: WriteService with DraftRepository with ArticleApiClient with LearningpathApiClient with ArticleIndexService =>

  object StateTransitionRules {
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
      (QUALITY_ASSURED            -> QUEUED_FOR_PUBLISHING)      keepStates Set(IMPORTED, USER_TEST) require SetPublishRoles keepCurrentOnTransition,
      (QUALITY_ASSURED            -> PUBLISHED)                  require AdminRoles keepStates Set(IMPORTED, USER_TEST) withSideEffect publishArticle keepCurrentOnTransition,
      (QUEUED_FOR_PUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle,
       QUEUED_FOR_PUBLISHING      -> DRAFT,
       PUBLISHED                  -> DRAFT,
      (PUBLISHED                  -> AWAITING_UNPUBLISHING)      keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) keepCurrentOnTransition,
      (PUBLISHED                  -> UNPUBLISHED)                keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect unpublishArticle,
       AWAITING_UNPUBLISHING      -> DRAFT,
      (AWAITING_UNPUBLISHING      -> PUBLISHED)                  keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle,
      (AWAITING_UNPUBLISHING      -> UNPUBLISHED)                keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect unpublishArticle,
      (UNPUBLISHED                -> PUBLISHED)                  keepStates Set(IMPORTED, USER_TEST, QUALITY_ASSURED) require AdminRoles withSideEffect publishArticle,
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

    private def publishArticle(article: domain.Article): Try[Article] = {
      val externalIds = draftRepository.getExternalIdsFromId(article.id.get)
      articleApiClient.updateArticle(article.id.get, article, externalIds)
    }

    private[this] def learningstepContainsArticleEmbed(articleId: Long, steps: LearningStep): Boolean = {
      val urls = steps.embedUrl.map(embed => Uri.parse(embed.url))
      val DirectArticleUrl = raw"""^.*/article/([0-9]+)$$""".r
      val TaxonomyUrl = raw"""^.+/(?:resource|topic):[0-9]:([0-9]+)$$""".r

      urls.exists(url => {
        url.pathRaw match {
          case DirectArticleUrl(f)     => f == s"$articleId"
          case TaxonomyUrl(externalId) => draftRepository.getIdFromExternalId(externalId).contains(articleId)
          case _                       => false
        }
      })
    }

    private[this] def learningPathsUsingArticle(articleId: Long): Seq[LearningPath] = {
      learningpathApiClient.getLearningpaths() match {
        case Success(learningpaths) =>
          learningpaths.filter(learningpath =>
            learningpath.learningsteps.exists(learningstepContainsArticleEmbed(articleId, _)))
        case Failure(_) =>
          Seq.empty
      }
    }

    private[service] def unpublishArticle(article: domain.Article): Try[domain.Article] = {
      val pathsUsingArticle = learningPathsUsingArticle(article.id.getOrElse(1)).map(_.id.getOrElse(-1))
      if (pathsUsingArticle.isEmpty)
        articleApiClient.unpublishArticle(article)
      else
        Failure(ValidationException(
          s"Learningpath(s) with id(s) ${pathsUsingArticle.mkString(",")} contains a learning step that uses this article"))
    }

    private def removeFromSearch(article: domain.Article): Try[domain.Article] =
      articleIndexService.deleteDocument(article.id.get).map(_ => article)

  }

}
