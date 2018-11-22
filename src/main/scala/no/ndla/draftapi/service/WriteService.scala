/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.io.ByteArrayInputStream
import java.util.Date

import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.domain.ArticleStatus.{DRAFT, PROPOSAL, PUBLISHED}
import no.ndla.draftapi.model.domain.Language.UnknownLanguage
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.{AgreementRepository, ConceptRepository, DraftRepository}
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService, ConceptIndexService}
import no.ndla.draftapi.validation.ContentValidator
import org.scalatra.servlet.FileItem
import math.max

import scala.util.{Failure, Random, Success, Try}

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
    with ReadService
    with ArticleApiClient
    with FileStorageService =>
  val writeService: WriteService

  class WriteService {

    def updateAgreement(agreementId: Long,
                        updatedAgreement: api.UpdatedAgreement,
                        user: UserInfo): Try[api.Agreement] = {
      agreementRepository.withId(agreementId) match {
        case None => Failure(NotFoundException(s"Agreement with id $agreementId does not exist"))
        case Some(existing) =>
          val toUpdate = existing.copy(
            title = updatedAgreement.title.getOrElse(existing.title),
            content = updatedAgreement.content.getOrElse(existing.content),
            copyright =
              updatedAgreement.copyright.map(c => converterService.toDomainCopyright(c)).getOrElse(existing.copyright),
            updated = clock.now(),
            updatedBy = user.id
          )

          val dateErrors = updatedAgreement.copyright
            .map(updatedCopyright => contentValidator.validateDates(updatedCopyright))
            .getOrElse(Seq.empty)

          for {
            _ <- contentValidator.validateAgreement(toUpdate, preExistingErrors = dateErrors)
            agreement <- agreementRepository.update(toUpdate)
            _ <- agreementIndexService.indexDocument(agreement)
          } yield converterService.toApiAgreement(agreement)
      }
    }

    def newAgreement(newAgreement: api.NewAgreement, user: UserInfo): Try[api.Agreement] = {
      val apiErrors = contentValidator.validateDates(newAgreement.copyright)

      val domainAgreement = converterService.toDomainAgreement(newAgreement, user)
      contentValidator.validateAgreement(domainAgreement, preExistingErrors = apiErrors) match {
        case Success(_) =>
          val agreement = agreementRepository.insert(domainAgreement)
          agreementIndexService.indexDocument(agreement)
          Success(converterService.toApiAgreement(agreement))
        case Failure(exception) => Failure(exception)
      }
    }

    def newArticle(newArticle: api.NewArticle,
                   externalIds: List[String],
                   externalSubjectIds: Seq[String],
                   user: UserInfo,
                   oldNdlaCreatedDate: Option[Date],
                   oldNdlaUpdatedDate: Option[Date],
                   importId: Option[String]): Try[api.Article] = {
      val insertNewArticleFunction = externalIds match {
        case Nil => draftRepository.insert _
        case nids =>
          (a: domain.Article) =>
            draftRepository.insertWithExternalIds(a, nids, externalSubjectIds, importId)
      }
      for {
        domainArticle <- converterService.toDomainArticle(newArticle,
                                                          externalIds,
                                                          user,
                                                          oldNdlaCreatedDate,
                                                          oldNdlaUpdatedDate)
        _ <- contentValidator.validateArticle(domainArticle, allowUnknownLanguage = false)
        insertedArticle <- Try(insertNewArticleFunction(domainArticle))
        _ <- articleIndexService.indexDocument(insertedArticle)
        apiArticle <- converterService.toApiArticle(insertedArticle, newArticle.language)
      } yield apiArticle
    }

    def updateArticleStatus(status: domain.ArticleStatus.Value,
                            id: Long,
                            user: UserInfo,
                            isImported: Boolean): Try[api.Article] = {
      draftRepository.withId(id) match {
        case None => Failure(NotFoundException(s"No article with id $id was found"))
        case Some(draft) =>
          for {
            convertedArticleT <- converterService.updateStatus(status, draft, user).attempt.unsafeRunSync().toTry
            convertedArticle <- convertedArticleT
            updatedArticle <- draftRepository.update(convertedArticle, isImported)
            apiArticle <- converterService.toApiArticle(updatedArticle, Language.AllLanguages, fallback = true)
          } yield apiArticle
      }
    }

    private def updateArticle(toUpdate: domain.Article,
                              importId: Option[String],
                              externalIds: List[String] = List.empty,
                              externalSubjectIds: Seq[String] = Seq.empty,
                              isImported: Boolean = false): Try[domain.Article] = {
      val updateFunc = externalIds match {
        case Nil =>
          (a: domain.Article) =>
            draftRepository.update(a, isImported = isImported)
        case nids =>
          (a: domain.Article) =>
            draftRepository.updateWithExternalIds(a, nids, externalSubjectIds, importId)
      }

      for {
        _ <- contentValidator.validateArticle(toUpdate, allowUnknownLanguage = true)
        domainArticle <- updateFunc(toUpdate)
        _ <- articleIndexService.indexDocument(domainArticle)
      } yield domainArticle
    }

    def updateArticle(articleId: Long,
                      updatedApiArticle: api.UpdatedArticle,
                      externalIds: List[String],
                      externalSubjectIds: Seq[String],
                      user: UserInfo,
                      oldNdlaCreatedDate: Option[Date],
                      oldNdlaUpdatedDate: Option[Date],
                      importId: Option[String]): Try[api.Article] = {
      draftRepository.withId(articleId) match {
        case Some(existing) =>
          val oldStatus = existing.status.current
          val newStatusIfUndefined = if (oldStatus == PUBLISHED) PROPOSAL else oldStatus
          val newStatusT =
            updatedApiArticle.status.map(ArticleStatus.valueOfOrError).getOrElse(Success(newStatusIfUndefined))

          for {
            convertedArticle <- converterService.toDomainArticle(existing,
                                                                 updatedApiArticle,
                                                                 externalIds.nonEmpty,
                                                                 user,
                                                                 oldNdlaCreatedDate,
                                                                 oldNdlaUpdatedDate)
            newStatus <- newStatusT
            withStatusT <- converterService
              .updateStatus(newStatus, convertedArticle, user)
              .attempt
              .unsafeRunSync()
              .toTry
            withStatus <- withStatusT
            updatedArticle <- updateArticle(withStatus,
                                            importId = importId,
                                            externalIds,
                                            externalSubjectIds,
                                            isImported = externalIds.nonEmpty)
            apiArticle <- converterService.toApiArticle(readService.addUrlsOnEmbedResources(updatedArticle),
                                                        updatedApiArticle.language.getOrElse(UnknownLanguage))
          } yield apiArticle

        case None if draftRepository.exists(articleId) =>
          for {
            convertedArticle <- converterService.toDomainArticle(articleId,
                                                                 updatedApiArticle,
                                                                 externalIds.nonEmpty,
                                                                 user,
                                                                 oldNdlaCreatedDate,
                                                                 oldNdlaUpdatedDate)
            withStatusT <- converterService
              .updateStatus(DRAFT, convertedArticle, user)
              .attempt
              .unsafeRunSync()
              .toTry
            withStatus <- withStatusT
            updatedArticle <- updateArticle(withStatus, importId, externalIds, externalSubjectIds)
            apiArticle <- converterService.toApiArticle(readService.addUrlsOnEmbedResources(updatedArticle),
                                                        updatedApiArticle.language.getOrElse(UnknownLanguage))
          } yield apiArticle
        case None => Failure(NotFoundException(s"Article with id $articleId does not exist"))
      }
    }

    def deleteLanguage(id: Long, language: String): Try[api.Article] = {
      draftRepository.withId(id) match {
        case Some(article) =>
          article.title.size match {
            case 1 => Failure(OperationNotAllowedException("Only one language left"))
            case _ =>
              val title = article.title.filter(_.language != language)
              val content = article.content.filter(_.language != language)
              val articleIntroduction = article.introduction.filter(_.language != language)
              val metaDescription = article.metaDescription.filter(_.language != language)
              val tags = article.tags.filter(_.language != language)
              val metaImage = article.metaImage.filter(_.language != language)
              val visualElement = article.visualElement.filter(_.language != language)
              val newArticle = article.copy(
                title = title,
                content = content,
                introduction = articleIntroduction,
                metaDescription = metaDescription,
                tags = tags,
                metaImage = metaImage,
                visualElement = visualElement
              )
              draftRepository
                .update(newArticle)
                .flatMap(
                  converterService.toApiArticle(_, Language.AllLanguages)
                )
          }
        case None => Failure(NotFoundException("Article does not exist"))
      }

    }

    def deleteArticle(id: Long): Try[api.ContentId] = {
      draftRepository
        .delete(id)
        .flatMap(articleIndexService.deleteDocument)
        .map(api.ContentId)
    }

    def publishConcept(id: Long): Try[domain.Concept] = {
      conceptRepository.withId(id) match {
        case Some(concept) =>
          articleApiClient.updateConcept(id, converterService.toArticleApiConcept(concept)) match {
            case Success(_)  => Success(concept)
            case Failure(ex) => Failure(ex)
          }
        case None => Failure(NotFoundException(s"Article with id $id does not exist"))
      }
    }

    def deleteConcept(id: Long): Try[api.ContentId] = {
      conceptRepository
        .delete(id)
        .flatMap(conceptIndexService.deleteDocument)
        .map(api.ContentId)
    }

    def newConcept(newConcept: NewConcept, externalId: String): Try[api.Concept] = {
      for {
        concept <- converterService.toDomainConcept(newConcept)
        _ <- importValidator.validate(concept)
        persistedConcept <- Try(conceptRepository.insertWithExternalId(concept, externalId))
        _ <- conceptIndexService.indexDocument(concept)
      } yield converterService.toApiConcept(persistedConcept, newConcept.language)
    }

    private def updateConcept(toUpdate: domain.Concept, externalId: Option[String] = None): Try[domain.Concept] = {
      val updateFunc = externalId match {
        case None => conceptRepository.update _
        case Some(nid) =>
          (a: domain.Concept) =>
            conceptRepository.updateWithExternalId(a, nid)
      }

      for {
        _ <- contentValidator.validate(toUpdate, allowUnknownLanguage = true)
        domainConcept <- updateFunc(toUpdate)
        _ <- conceptIndexService.indexDocument(domainConcept)
      } yield domainConcept
    }

    def updateConcept(id: Long, updatedConcept: api.UpdatedConcept, externalId: Option[String]): Try[api.Concept] = {
      conceptRepository.withId(id) match {
        case Some(concept) =>
          val domainConcept = converterService.toDomainConcept(concept, updatedConcept)
          updateConcept(domainConcept, externalId)
            .map(x => converterService.toApiConcept(x, updatedConcept.language))
        case None if conceptRepository.exists(id) =>
          val concept = converterService.toDomainConcept(id, updatedConcept)
          updateConcept(concept, externalId)
            .map(concept => converterService.toApiConcept(concept, updatedConcept.language))
        case None => Failure(NotFoundException(s"Concept with id $id does not exist"))
      }
    }

    private[service] def mergeLanguageFields[A <: LanguageField](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }

    def newEmptyArticle(externalIds: List[String], externalSubjectIds: Seq[String]): Try[Long] = {
      articleApiClient
        .allocateArticleId(externalIds, externalSubjectIds)
        .flatMap(id => draftRepository.newEmptyArticle(id, externalIds, externalSubjectIds))
    }

    def newEmptyConcept(externalIds: List[String]): Try[Long] = {
      articleApiClient
        .allocateConceptId(externalIds)
        .flatMap(id => conceptRepository.newEmptyConcept(id, externalIds))
    }

    def storeFile(file: FileItem): Try[api.UploadedFile] =
      uploadFile(file).map(f => api.UploadedFile(f.fileName, f.contentType, f.fileExtension, s"/files/${f.filePath}"))

    private[service] def getFileExtension(fileName: String): Option[String] =
      fileName.lastIndexOf(".") match {
        case index: Int if index > -1 => Some(fileName.substring(index))
        case _                        => None
      }

    private[service] def uploadFile(file: FileItem): Try[domain.UploadedFile] = {
      val fileExtension = getFileExtension(file.name)
      val contentType = file.getContentType.getOrElse("")
      val fileName = Stream
        .continually(randomFilename(fileExtension.getOrElse("")))
        .dropWhile(fileStorage.resourceExists)
        .head

      fileStorage
        .uploadResourceFromStream(new ByteArrayInputStream(file.get), fileName, contentType, file.size)
        .map(uploadPath => domain.UploadedFile(fileName, uploadPath, file.size, contentType, fileExtension))
    }

    private[service] def randomFilename(extension: String, length: Int = 20): String = {
      val extensionWithDot =
        if (!extension.headOption.contains('.') && extension.length > 0) s".$extension" else extension
      val randomString = Random.alphanumeric.take(max(length - extensionWithDot.length, 1)).mkString
      s"$randomString$extensionWithDot"
    }
  }
}
