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
import no.ndla.draftapi.integration.{ArticleApiClient, SearchApiClient, TaxonomyApiClient}
import no.ndla.draftapi.model.api.{Article, _}
import no.ndla.draftapi.model.domain.ArticleStatus.{DRAFT, PROPOSAL, PUBLISHED}
import no.ndla.draftapi.model.domain.Language.UnknownLanguage
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.{AgreementRepository, DraftRepository}
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService}
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.draftapi.DraftApiProperties.supportedUploadExtensions
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.scalatra.servlet.FileItem
import io.lemonlabs.uri.dsl._

import math.max
import scala.util.{Failure, Random, Success, Try}

trait WriteService {
  this: DraftRepository
    with AgreementRepository
    with ConverterService
    with ContentValidator
    with ArticleIndexService
    with AgreementIndexService
    with Clock
    with ReadService
    with ArticleApiClient
    with SearchApiClient
    with FileStorageService
    with TaxonomyApiClient =>
  val writeService: WriteService

  class WriteService {

    def copyArticleFromId(
        articleId: Long,
        userInfo: UserInfo,
        language: String,
        fallback: Boolean,
        usePostFix: Boolean
    ): Try[api.Article] = {
      draftRepository.withId(articleId) match {
        case None => Failure(NotFoundException(s"Article with id '$articleId' was not found in database."))
        case Some(article) =>
          for {
            newId <- draftRepository.newArticleId()
            status = domain.Status(DRAFT, Set.empty)
            notes <- converterService.newNotes(Seq(s"Opprettet artikkel, som kopi av artikkel med id: '$articleId'."),
                                               userInfo,
                                               status)
            newTitles = if (usePostFix) article.title.map(t => t.copy(title = t.title + " (Kopi)")) else article.title
            articleToInsert = article.copy(
              id = Some(newId.toLong),
              title = newTitles,
              revision = Some(1),
              updated = clock.now(),
              created = clock.now(),
              published = clock.now(),
              updatedBy = userInfo.id,
              status = status,
              notes = notes
            )
            inserted = draftRepository.insert(articleToInsert)
            _ <- articleIndexService.indexDocument(inserted)
            _ <- Try(searchApiClient.indexDraft(inserted))
            enriched = readService.addUrlsOnEmbedResources(inserted)
            converted <- converterService.toApiArticle(enriched, language, fallback)
          } yield converted
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
        _ <- updateTaxonomyForArticle(domainArticle)
      } yield domainArticle
    }

    private def updateTaxonomyForArticle(article: domain.Article) = {
      article.id match {
        case Some(id) => taxonomyApiClient.updateTaxonomyIfExists(id, article).map(_ => article)
        case None =>
          Failure(ArticleVersioningException("Article supplied to taxonomy update did not have an id. This is a bug."))
      }
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

    /** Article status should not be updated if notes and/or editorLabels are the only changes */
    private def shouldUpdateStatus(changedArticle: domain.Article, existingArticle: domain.Article): Boolean = {
      // Function that sets values we don't want to include when comparing articles to check if we should update status
      val withComparableValues =
        (article: domain.Article) =>
          article.copy(
            revision = None,
            notes = Seq.empty,
            editorLabels = Seq.empty,
            created = new Date(0),
            updated = new Date(0),
            updatedBy = ""
        )

      withComparableValues(changedArticle) != withComparableValues(existingArticle)
    }

    private def updateStatusIfNeeded(convertedArticle: domain.Article,
                                     existingArticle: domain.Article,
                                     updatedApiArticle: api.UpdatedArticle,
                                     user: UserInfo): Try[domain.Article] = {
      if (!shouldUpdateStatus(convertedArticle, existingArticle)) {
        Success(convertedArticle)
      } else {
        val oldStatus = existingArticle.status.current
        val newStatusIfUndefined = if (oldStatus == PUBLISHED) PROPOSAL else oldStatus

        updatedApiArticle.status
          .map(ArticleStatus.valueOfOrError)
          .getOrElse(Success(newStatusIfUndefined))
          .flatMap(
            newStatus =>
              converterService
                .updateStatus(newStatus, convertedArticle, user, false)
                .attempt
                .unsafeRunSync()
                .toTry
          )
          .flatten
      }
    }

    def updateArticle(articleId: Long,
                      updatedApiArticle: api.UpdatedArticle,
                      externalIds: List[String],
                      externalSubjectIds: Seq[String],
                      user: UserInfo,
                      oldNdlaCreatedDate: Option[Date],
                      oldNdlaUpdatedDate: Option[Date],
                      importId: Option[String]): Try[api.Article] =
      draftRepository.withId(articleId) match {
        case Some(existing) =>
          updateExistingArticle(
            existing,
            updatedApiArticle,
            externalIds,
            externalSubjectIds,
            user,
            oldNdlaCreatedDate,
            oldNdlaUpdatedDate,
            importId
          )
        case None if draftRepository.exists(articleId) =>
          updateNullDocumentArticle(
            articleId,
            updatedApiArticle,
            externalIds,
            externalSubjectIds,
            user,
            oldNdlaCreatedDate,
            oldNdlaUpdatedDate,
            importId
          )
        case None =>
          Failure(NotFoundException(s"Article with id $articleId does not exist"))
      }

    private def updateExistingArticle(existing: domain.Article,
                                      updatedApiArticle: api.UpdatedArticle,
                                      externalIds: List[String],
                                      externalSubjectIds: Seq[String],
                                      user: UserInfo,
                                      oldNdlaCreatedDate: Option[Date],
                                      oldNdlaUpdatedDate: Option[Date],
                                      importId: Option[String]) = {

      for {
        convertedArticle <- converterService.toDomainArticle(
          existing,
          updatedApiArticle,
          externalIds.nonEmpty,
          user,
          oldNdlaCreatedDate,
          oldNdlaUpdatedDate
        )
        articleWithStatus <- updateStatusIfNeeded(convertedArticle, existing, updatedApiArticle, user)
        updatedArticle <- updateArticle(
          articleWithStatus,
          importId,
          externalIds,
          externalSubjectIds,
          isImported = externalIds.nonEmpty,
          shouldValidateLanguage = updatedApiArticle.language
        )
        apiArticle <- converterService.toApiArticle(readService.addUrlsOnEmbedResources(updatedArticle),
                                                    updatedApiArticle.language.getOrElse(UnknownLanguage),
                                                    updatedApiArticle.language.isEmpty)
      } yield apiArticle
    }

    private def updateNullDocumentArticle(articleId: Long,
                                          updatedApiArticle: api.UpdatedArticle,
                                          externalIds: List[String],
                                          externalSubjectIds: Seq[String],
                                          user: UserInfo,
                                          oldNdlaCreatedDate: Option[Date],
                                          oldNdlaUpdatedDate: Option[Date],
                                          importId: Option[String]): Try[Article] =
      for {
        convertedArticle <- converterService.toDomainArticle(
          articleId,
          updatedApiArticle,
          externalIds.nonEmpty,
          user,
          oldNdlaCreatedDate,
          oldNdlaUpdatedDate
        )
        articleWithStatusT <- converterService
          .updateStatus(DRAFT, convertedArticle, user, false)
          .attempt
          .unsafeRunSync()
          .toTry
        articleWithStatus <- articleWithStatusT
        updatedArticle <- updateArticle(
          articleWithStatus,
          importId,
          externalIds,
          externalSubjectIds,
          shouldValidateLanguage = updatedApiArticle.language
        )
        apiArticle <- converterService.toApiArticle(readService.addUrlsOnEmbedResources(updatedArticle),
                                                    updatedApiArticle.language.getOrElse(UnknownLanguage))
      } yield apiArticle

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

    private[service] def mergeLanguageFields[A <: LanguageField](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }

    def newEmptyArticle(externalIds: List[String], externalSubjectIds: Seq[String]): Try[Long] = {
      draftRepository.newArticleId().flatMap(id => draftRepository.newEmptyArticle(id, externalIds, externalSubjectIds))
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

    private[service] def getFilePathFromUrl(filePath: String) = {
      filePath.path.parts
        .dropWhile(_ == "files")
        .mkString("/")
    }

    def deleteFile(fileUrlOrPath: String): Try[_] = {
      val filePath = getFilePathFromUrl(fileUrlOrPath)
      if (fileStorage.resourceWithPathExists(filePath)) {
        fileStorage.deleteResourceWithPath(filePath)
      } else {
        Failure(NotFoundException(s"Could not find file with file path '$filePath' in storage."))
      }
    }

    private[service] def uploadFile(file: FileItem): Try[domain.UploadedFile] = {
      getFileExtension(file.name).flatMap(fileExtension => {
        val contentType = file.getContentType.getOrElse("")
        val fileName = LazyList
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
