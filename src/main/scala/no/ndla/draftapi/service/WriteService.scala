/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.auth.{Role, User}
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.{AccessDeniedException, Article, ArticleStatusException, NotFoundException}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service.search.ArticleIndexService
import no.ndla.draftapi.validation.ContentValidator
import domain.ArticleStatus._
import scala.util.{Failure, Success, Try}

trait WriteService {
  this: DraftRepository
    with ConverterService
    with ContentValidator
    with ArticleIndexService
    with Clock
    with User
    with ReadService
    with ArticleApiClient
    with Role
    with ArticleApiClient =>
  val writeService: WriteService

  class WriteService {
    def newArticle(newArticle: api.NewArticle): Try[Article] = {
      for {
        domainArticle <- converterService.toDomainArticle(newArticle)
        _ <- contentValidator.validateArticle(domainArticle, allowUnknownLanguage = false)
        insertedArticle <- Try(draftRepository.insert(domainArticle))
        _ <- articleIndexService.indexDocument(insertedArticle)
        apiArticle <- Success(converterService.toApiArticle(insertedArticle, newArticle.language))
      } yield apiArticle
    }

    private def updateArticle(toUpdate: domain.Article): Try[domain.Article] = {
      for {
        _ <- contentValidator.validateArticle(toUpdate, allowUnknownLanguage = true)
        domainArticle <- draftRepository.update(toUpdate)
        _ <- articleIndexService.indexDocument(domainArticle)
      } yield domainArticle
    }

    def updateArticle(articleId: Long, updatedApiArticle: api.UpdatedArticle): Try[api.Article] = {
      withIdAndAccessGranted(articleId, authRole.updateDraftRoles) match {
        case Success(existing) =>
          val lang = updatedApiArticle.language
          val toUpdate = existing.copy(
            revision = Option(updatedApiArticle.revision),
            title = mergeLanguageFields(existing.title, updatedApiArticle.title.toSeq.map(t => converterService.toDomainTitle(api.ArticleTitle(t, lang)))),
            content = mergeLanguageFields(existing.content, updatedApiArticle.content.toSeq.map(c => converterService.toDomainContent(api.ArticleContent(c, lang)))),
            copyright = updatedApiArticle.copyright.map(converterService.toDomainCopyright).orElse(existing.copyright),
            tags = mergeLanguageFields(existing.tags, converterService.toDomainTag(updatedApiArticle.tags, lang)),
            requiredLibraries = updatedApiArticle.requiredLibraries.map(converterService.toDomainRequiredLibraries),
            visualElement = mergeLanguageFields(existing.visualElement, updatedApiArticle.visualElement.map(c => converterService.toDomainVisualElement(c, lang)).toSeq),
            introduction = mergeLanguageFields(existing.introduction, updatedApiArticle.introduction.map(i => converterService.toDomainIntroduction(i, lang)).toSeq),
            metaDescription = mergeLanguageFields(existing.metaDescription, updatedApiArticle.metaDescription.map(m => converterService.toDomainMetaDescription(m, lang)).toSeq),
            metaImageId = if (updatedApiArticle.metaImageId.isDefined) updatedApiArticle.metaImageId else existing.metaImageId,
            updated = clock.now(),
            updatedBy = authUser.id(),
            articleType = updatedApiArticle.articleType.flatMap(ArticleType.valueOf).orElse(existing.articleType)
          )

          updateArticle(toUpdate)
            .map(article => converterService.toApiArticle(readService.addUrlsOnEmbedResources(article), updatedApiArticle.language))
        case Failure(ex) => Failure(ex)
      }
    }

    def updateArticleStatus(id: Long, status: api.ArticleStatus): Try[api.Article] = {
      val domainStatus = converterService.toDomainStatus(status) match {
        case Success(st) => st
        case Failure(ex) => return Failure(ex)
      }

      withIdAndAccessGranted(id, authRole.requiredRolesForStatusUpdate(domainStatus, _)) match {
        case Success(ex) => updateArticle(ex.copy(status = domainStatus))
          .map(article => converterService.toApiArticle(readService.addUrlsOnEmbedResources(article), Language.DefaultLanguage))
        case Failure(ex) => Failure(ex)
      }
    }

    def publishArticle(id: Long): Try[domain.Article] = {
      withIdAndAccessGranted(id, authRole.publishToArticleApiRoles) match {
        case Success(article) if article.status.contains(QUEUED_FOR_PUBLISHING) =>
          ArticleApiClient.updateArticle(id, converterService.toArticleApiArticle(article)) match {
            case Success(_) =>
              updateArticle(article.copy(status=article.status.filter(_ != QUEUED_FOR_PUBLISHING)))
            case Failure(ex) => Failure(ex)
          }
        case Success(_) => Failure(new ArticleStatusException(s"Article with id $id is not marked for publishing"))
        case Failure(ex) => Failure(ex)
      }
    }

    private[service] def mergeLanguageFields[A <: LanguageField[_]](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }

    private def withIdAndAccessGranted(id: Long, requiredRoles: Set[String]): Try[domain.Article] = {
      if (!authRole.hasRoles(requiredRoles)) {
        Failure(new AccessDeniedException("User is missing required role(s) to perform this action"))
      } else {
        draftRepository.withId(id) match {
          case Some(a) => Success(a)
          case None => Failure(NotFoundException(s"Article with id $id does not exist"))
        }
      }
    }

    private def withIdAndAccessGranted(id: Long, requiredRoles: domain.Article => Set[String]): Try[domain.Article] = {
      draftRepository.withId(id) match {
        case Some(a) if !authRole.hasRoles(requiredRoles(a)) =>
          Failure(new AccessDeniedException("User is missing required role(s) to perform this action"))
        case Some(a) => Success(a)
        case None => Failure(NotFoundException(s"Article with id $id does not exist"))
      }
    }

  }
}
