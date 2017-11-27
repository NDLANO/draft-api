/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.auth.{Role, User}
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.{Article, ArticleStatusException, NewConcept, NotFoundException}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.{AgreementRepository, ConceptRepository, DraftRepository}
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService, ConceptIndexService}
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.draftapi.model.domain.ArticleStatus.QUEUED_FOR_PUBLISHING
import scala.util.{Failure, Success, Try}

trait WriteService {
  this: DraftRepository
    with ConceptRepository
    with AgreementRepository
    with ConverterService
    with ContentValidator
    with ArticleIndexService
    with AgreementIndexService
    with ConceptIndexService
    with Clock
    with User
    with ReadService
    with ArticleApiClient
    with Role
    with ArticleApiClient =>
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
            updatedBy = authUser.id()
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
      draftRepository.withId(articleId) match {
        case Some(existing) =>
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
            articleType = updatedApiArticle.articleType.map(ArticleType.valueOfOrError).orElse(existing.articleType)
          )

          updateArticle(toUpdate)
            .map(article => converterService.toApiArticle(readService.addUrlsOnEmbedResources(article), updatedApiArticle.language))
        case None => Failure(NotFoundException(s"Article with id $articleId does not exist"))
      }
    }

    def queueArticleForPublish(id: Long): Try[Long] = {
      draftRepository.withId(id) match {
        case Some(a) => draftRepository.update(a.copy(status=a.status ++ Set(QUEUED_FOR_PUBLISHING))).map(_ => id)
        case None => Failure(NotFoundException(s"The article with id $id does not exist"))
      }
    }

    def publishArticle(id: Long): Try[domain.Article] = {
      draftRepository.withId(id) match {
        case Some(article) if article.status.contains(QUEUED_FOR_PUBLISHING) =>
          ArticleApiClient.updateArticle(id, converterService.toArticleApiArticle(article)) match {
            case Success(_) =>
              updateArticle(article.copy(status=article.status.filter(_ != QUEUED_FOR_PUBLISHING)))
            case Failure(ex) => Failure(ex)
          }
        case Some(_) => Failure(new ArticleStatusException(s"Article with id $id is not marked for publishing"))
        case None => Failure(NotFoundException(s"Article with id $id does not exist"))
      }
    }

    def newConcept(newConcept: NewConcept, externalId: String): Try[api.Concept] = {
      val concept = converterService.toDomainConcept(newConcept)
      for {
        _ <- importValidator.validate(concept)
        persistedConcept <- Try(conceptRepository.insertWithExternalId(concept, externalId))
        _ <- conceptIndexService.indexDocument(concept)
      } yield converterService.toApiConcept(persistedConcept, newConcept.language)
    }

    def updateConcept(id: Long, updateConcept: api.UpdatedConcept): Try[api.Concept] = {
      conceptRepository.withId(id) match {
        case None => Failure(NotFoundException(s"Concept with id $id does not exist"))
        case Some(concept) =>
          val domainConcept = converterService.toDomainConcept(concept, updateConcept)
          for {
            _ <- importValidator.validate(domainConcept)
            persistedConcept <- conceptRepository.update(domainConcept, id)
            _ <- conceptIndexService.indexDocument(concept)
          } yield converterService.toApiConcept(persistedConcept, updateConcept.language)
      }
    }

    private[service] def mergeLanguageFields[A <: LanguageField[_]](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }

  }
}
