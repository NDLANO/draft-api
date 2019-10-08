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
import no.ndla.draftapi.integration.{ArticleApiClient, SearchApiClient}
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.domain.ArticleStatus.{DRAFT, PROPOSAL, PUBLISHED}
import no.ndla.draftapi.model.domain.Language.UnknownLanguage
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.{AgreementRepository, ConceptRepository, DraftRepository}
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService, ConceptIndexService}
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.draftapi.DraftApiProperties.supportedUploadExtensions
import no.ndla.validation.{ValidationException, ValidationMessage}
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
    with SearchApiClient
    with FileStorageService =>
  val writeService: WriteService

  class WriteService {

    def copyArticleFromId(
        articleId: Long,
        userInfo: UserInfo,
        language: String,
        fallback: Boolean
    ): Try[api.Article] = {
      draftRepository.withId(articleId) match {
        case None => Failure(NotFoundException(s"Article with id '$articleId' was not found in database."))
        case Some(article) =>
          val x = for {
            newId <- draftRepository.newArticleId()
            status = domain.Status(DRAFT, Set.empty)
            newNotes <- converterService.newNotes(
              Seq(s"Opprettet artikkel, som kopi av artikkel med id: '$articleId'."),
              userInfo,
              status)
            articleToInsert = article.copy(
              id = Some(newId.toLong),
              revision = Some(1),
              updated = clock.now(),
              created = clock.now(),
              published = clock.now(),
              updatedBy = userInfo.id,
              status = status,
              notes = article.notes ++ newNotes
            )
            inserted = draftRepository.insert(articleToInsert)
            _ <- articleIndexService.indexDocument(inserted)
            _ <- Try(searchApiClient.indexDraft(inserted))
            converted <- converterService.toApiArticle(inserted, language, fallback)
          } yield converted
          x
      }
    }

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
      val newNotes = "Opprettet artikkel." +: newArticle.notes
      val visualElement = newArticle.visualElement.filter(_.nonEmpty)
      val withNotes = newArticle.copy(
        notes = newNotes,
        visualElement = visualElement
      )
      val insertNewArticleFunction = externalIds match {
        case Nil => draftRepository.insert _
        case nids =>
          (a: domain.Article) =>
            draftRepository.insertWithExternalIds(a, nids, externalSubjectIds, importId)
      }
      for {
        domainArticle <- converterService.toDomainArticle(withNotes,
                                                          externalIds,
                                                          user,
                                                          oldNdlaCreatedDate,
                                                          oldNdlaUpdatedDate)
        _ <- contentValidator.validateArticle(domainArticle, allowUnknownLanguage = false)
        insertedArticle <- Try(insertNewArticleFunction(domainArticle))
        _ <- articleIndexService.indexDocument(insertedArticle)
        _ <- Try(searchApiClient.indexDraft(insertedArticle))
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
            convertedArticleT <- converterService
              .updateStatus(status, draft, user, isImported)
              .attempt
              .unsafeRunSync()
              .toTry
            convertedArticle <- convertedArticleT
            updatedArticle <- draftRepository.updateArticle(convertedArticle, isImported)
            _ <- articleIndexService.indexDocument(updatedArticle)
            _ <- Try(searchApiClient.indexDraft(updatedArticle))
            apiArticle <- converterService.toApiArticle(updatedArticle, Language.AllLanguages, fallback = true)
          } yield apiArticle
      }
    }

    private def updateArticle(toUpdate: domain.Article,
                              importId: Option[String],
                              externalIds: List[String] = List.empty,
                              externalSubjectIds: Seq[String] = Seq.empty,
                              shouldValidateLanguage: Option[String],
                              isImported: Boolean = false): Try[domain.Article] = {
      val updateFunc = externalIds match {
        case Nil =>
          (a: domain.Article) =>
            draftRepository.updateArticle(a, isImported = isImported)
        case nids =>
          (a: domain.Article) =>
            draftRepository.updateWithExternalIds(a, nids, externalSubjectIds, importId)
      }

      val articleToValidate = shouldValidateLanguage match {
        case Some(language) => getArticleOnLanguage(toUpdate, language)
        case None           => toUpdate
      }
      for {
        _ <- contentValidator.validateArticle(articleToValidate, allowUnknownLanguage = true)
        domainArticle <- updateFunc(toUpdate)
        _ <- articleIndexService.indexDocument(domainArticle)
        _ <- Try(searchApiClient.indexDraft(domainArticle))
      } yield domainArticle
    }

    def getArticleOnLanguage(article: domain.Article, language: String): domain.Article = {
      article.copy(
        content = article.content.filter(_.language == language),
        introduction = article.introduction.filter(_.language == language),
        metaDescription = article.metaDescription.filter(_.language == language),
        title = article.title.filter(_.language == language),
        tags = article.tags.filter(_.language == language),
        visualElement = article.visualElement.filter(_.language == language),
        metaImage = article.metaImage.filter(_.language == language)
      )
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
              .updateStatus(newStatus, convertedArticle, user, false)
              .attempt
              .unsafeRunSync()
              .toTry
            withStatus <- withStatusT
            updatedArticle <- updateArticle(
              withStatus,
              importId = importId,
              externalIds,
              externalSubjectIds,
              isImported = externalIds.nonEmpty,
              shouldValidateLanguage = updatedApiArticle.language
            )
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
              .updateStatus(DRAFT, convertedArticle, user, false)
              .attempt
              .unsafeRunSync()
              .toTry
            withStatus <- withStatusT
            updatedArticle <- updateArticle(withStatus,
                                            importId,
                                            externalIds,
                                            externalSubjectIds,
                                            shouldValidateLanguage = updatedApiArticle.language)
            apiArticle <- converterService.toApiArticle(readService.addUrlsOnEmbedResources(updatedArticle),
                                                        updatedApiArticle.language.getOrElse(UnknownLanguage))
          } yield apiArticle
        case None => Failure(NotFoundException(s"Article with id $articleId does not exist"))
      }
    }

    def deleteLanguage(id: Long, language: String, userInfo: UserInfo): Try[api.Article] = {
      draftRepository.withId(id) match {
        case Some(article) =>
          article.title.size match {
            case 1 => Failure(OperationNotAllowedException("Only one language left"))
            case _ =>
              converterService
                .deleteLanguage(article, language, userInfo)
                .flatMap(
                  newArticle =>
                    draftRepository
                      .updateArticle(newArticle)
                      .flatMap(
                        converterService.toApiArticle(_, Language.AllLanguages)
                    ))
          }
        case None => Failure(NotFoundException("Article does not exist"))
      }

    }

    def deleteArticle(id: Long): Try[api.ContentId] = {
      draftRepository
        .deleteArticle(id)
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
      draftRepository.newArticleId().flatMap(id => draftRepository.newEmptyArticle(id, externalIds, externalSubjectIds))
    }

    def newEmptyConcept(externalIds: List[String]): Try[Long] = {
      articleApiClient
        .allocateConceptId(externalIds)
        .flatMap(id => conceptRepository.newEmptyConcept(id, externalIds))
    }

    def storeFile(file: FileItem): Try[api.UploadedFile] =
      uploadFile(file).map(f => api.UploadedFile(f.fileName, f.contentType, f.fileExtension, s"/files/${f.filePath}"))

    private[service] def getFileExtension(fileName: String): Try[String] = {
      val badExtensionError =
        new ValidationException(
          errors = Seq(ValidationMessage(
            "file",
            s"The file must have one of the supported file extensions: '${supportedUploadExtensions.mkString(", ")}'")))

      fileName.lastIndexOf(".") match {
        case index: Int if index > -1 =>
          supportedUploadExtensions.find(_ == fileName.substring(index).toLowerCase) match {
            case Some(e) => Success(e)
            case _       => Failure(badExtensionError)
          }
        case _ => Failure(badExtensionError)

      }
    }

    private[service] def uploadFile(file: FileItem): Try[domain.UploadedFile] = {
      getFileExtension(file.name).flatMap(fileExtension => {
        val contentType = file.getContentType.getOrElse("")
        val fileName = Stream
          .continually(randomFilename(fileExtension))
          .dropWhile(fileStorage.resourceExists)
          .head

        fileStorage
          .uploadResourceFromStream(new ByteArrayInputStream(file.get), fileName, contentType, file.size)
          .map(uploadPath => domain.UploadedFile(fileName, uploadPath, file.size, contentType, fileExtension))
      })
    }

    private[service] def randomFilename(extension: String, length: Int = 20): String = {
      val extensionWithDot =
        if (!extension.headOption.contains('.') && extension.length > 0) s".$extension" else extension
      val randomString = Random.alphanumeric.take(max(length - extensionWithDot.length, 1)).mkString
      s"$randomString$extensionWithDot"
    }
  }
}
