/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.auth.User
import no.ndla.draftapi.model.api.{Article, ArticleStatus, NotFoundException}
import no.ndla.draftapi.model.domain.{LanguageField, _}
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.{AgreementRepository, DraftRepository}
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService}
import no.ndla.draftapi.validation.ContentValidator

import scala.util.{Failure, Success, Try}

trait WriteService {
  this: DraftRepository with AgreementRepository with ConverterService with ContentValidator with ArticleIndexService with AgreementIndexService with Clock with User with ReadService =>
  val writeService: WriteService

  class WriteService {

    def updateAgreement(agreementId: Long, updatedAgreement: api.UpdatedAgreement): Try[api.Agreement] = {
      agreementRepository.withId(agreementId) match {
        case None => Failure(NotFoundException(s"Agreement with id $agreementId does not exist"))
        case Some(existing) =>
          val toUpdate = existing.copy(
            title = updatedAgreement.title.getOrElse(existing.title),
            content = updatedAgreement.content.getOrElse(existing.content),
            copyright = updatedAgreement.copyright.map(c => converterService.toDomainCopyright(c)).getOrElse(existing.copyright),
            updated = clock.now(),
            updatedBy = authUser.userOrClientId()
          )

          for {
            _ <- contentValidator.validateAgreement(toUpdate)
            agreement <- agreementRepository.update(toUpdate)
            _ <- agreementIndexService.indexDocument(agreement)
          } yield converterService.toApiAgreement(agreement)
      }
    }

    def newAgreement(newAgreement: api.NewAgreement): Try[api.Agreement] = {
      val domainAgreement = converterService.toDomainAgreement(newAgreement)
      contentValidator.validateAgreement(domainAgreement) match {
        case Success(_) =>
          val agreement = agreementRepository.insert(domainAgreement)
          agreementIndexService.indexDocument(agreement)
          Success(converterService.toApiAgreement(agreement))
        case Failure(exception) => Failure(exception)
      }
    }

    def newArticle(newArticle: api.NewArticle): Try[Article] = {
      val domainArticle = converterService.toDomainArticle(newArticle)
      contentValidator.validateArticle(domainArticle, false) match {
        case Success(_) => {
          val article = draftRepository.insert(domainArticle)
          articleIndexService.indexDocument(article)
          Success(converterService.toApiArticle(article, newArticle.language))
        }
        case Failure(exception) => Failure(exception)
      }
    }

    private def updateArticle(toUpdate: domain.Article): Try[domain.Article] = {
      for {
        _ <- contentValidator.validateArticle(toUpdate, allowUnknownLanguage = true)
        article <- draftRepository.update(toUpdate)
        _ <- articleIndexService.indexDocument(article)
      } yield article
    }

    def updateArticle(articleId: Long, updatedApiArticle: api.UpdatedArticle): Try[api.Article] = {
      val article = draftRepository.withId(articleId) match {
        case None => Failure(NotFoundException(s"Article with id $articleId does not exist"))
        case Some(existing) =>
          val lang = updatedApiArticle.language

          val toUpdate = existing.copy(
            revision = Option(updatedApiArticle.revision),
            title = mergeLanguageFields(existing.title, updatedApiArticle.title.toSeq.map(t => converterService.toDomainTitle(api.ArticleTitle(t, lang)))),
            content = mergeLanguageFields(existing.content, updatedApiArticle.content.toSeq.map(c => converterService.toDomainContent(api.ArticleContent(c, lang)))),
            copyright = updatedApiArticle.copyright.map(c => converterService.toDomainCopyright(c)).orElse(existing.copyright),
            tags = mergeLanguageFields(existing.tags, converterService.toDomainTag(updatedApiArticle.tags, lang)),
            requiredLibraries = updatedApiArticle.requiredLibraries.map(converterService.toDomainRequiredLibraries),
            visualElement = mergeLanguageFields(existing.visualElement, updatedApiArticle.visualElement.map(c => converterService.toDomainVisualElement(c, lang)).toSeq),
            introduction = mergeLanguageFields(existing.introduction, updatedApiArticle.introduction.map(i => converterService.toDomainIntroduction(i, lang)).toSeq),
            metaDescription = mergeLanguageFields(existing.metaDescription, updatedApiArticle.metaDescription.map(m => converterService.toDomainMetaDescription(m, lang)).toSeq),
            metaImageId = if (updatedApiArticle.metaImageId.isDefined) updatedApiArticle.metaImageId else existing.metaImageId,
            updated = clock.now(),
            updatedBy = authUser.userOrClientId()
          )

          updateArticle(toUpdate)
      }

      article.map(article => converterService.toApiArticle(readService.addUrlsOnEmbedResources(article), updatedApiArticle.language))
    }

    def updateArticleStatus(id: Long, status: ArticleStatus): Try[api.Article] = {
      val domainStatuses = converterService.toDomainStatus(status) match {
        case Failure(ex) => return Failure(ex)
        case Success(s) =>
          contentValidator.validateUserAbleToSetStatus(s) match {
            case Failure(ex) => return Failure(ex)
            case Success(_) => s
          }
      }

      val article = draftRepository.withId(id) match {
        case None => Failure(NotFoundException(s"Article with id $id does not exist"))
        case Some(existing) => updateArticle(existing.copy(status = domainStatuses))
      }

      article.map(article => converterService.toApiArticle(readService.addUrlsOnEmbedResources(article), Language.DefaultLanguage))
    }

    private[service] def mergeLanguageFields[A <: LanguageField[_]](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }
  }

}
