/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.validation

import no.ndla.draftapi.DraftApiProperties.{H5PResizerScriptUrl, NDLABrightcoveVideoScriptUrl, NRKVideoScriptUrl}
import no.ndla.draftapi.auth.{Role, UserInfo}
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.{
  AccessDeniedException,
  ContentId,
  NewAgreement,
  NewAgreementCopyright,
  NotFoundException,
  UpdatedArticle
}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service.ConverterService
import no.ndla.mapping.ISO639.get6391CodeFor6392CodeMappings
import no.ndla.mapping.License.getLicense
import no.ndla.network.AuthUser
import org.joda.time.format.ISODateTimeFormat
import no.ndla.validation._

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

trait ContentValidator {
  this: DraftRepository with ConverterService with ArticleApiClient =>
  val contentValidator: ContentValidator
  val importValidator: ContentValidator

  class ContentValidator(allowEmptyLanguageField: Boolean) {
    private val NoHtmlValidator = new TextValidator(allowHtml = false)
    private val HtmlValidator = new TextValidator(allowHtml = true)

    def validate(content: Content, allowUnknownLanguage: Boolean = false): Try[Content] = {
      content match {
        case article: Article     => validateArticle(article, allowUnknownLanguage)
        case agreement: Agreement => validateAgreement(agreement)
      }
    }

    def validateAgreement(agreement: Agreement,
                          preExistingErrors: Seq[ValidationMessage] = Seq.empty): Try[Agreement] = {
      val validationErrors = NoHtmlValidator.validate("title", agreement.title).toList ++
        NoHtmlValidator.validate("content", agreement.content).toList ++
        preExistingErrors.toList ++
        validateAgreementCopyright(agreement.copyright)

      if (validationErrors.isEmpty) {
        Success(agreement)
      } else {
        Failure(new ValidationException(errors = validationErrors))
      }
    }

    def validateDates(newCopyright: NewAgreementCopyright): Seq[ValidationMessage] = {
      newCopyright.validFrom.map(dateString => validateDate("copyright.validFrom", dateString)).toSeq.flatten ++
        newCopyright.validTo.map(dateString => validateDate("copyright.validTo", dateString)).toSeq.flatten
    }

    def validateDate(fieldName: String, dateString: String): Seq[ValidationMessage] = {
      val parser = ISODateTimeFormat.dateOptionalTimeParser()
      Try(parser.parseDateTime(dateString)) match {
        case Success(_) => Seq.empty
        case Failure(_) => Seq(ValidationMessage(fieldName, "Date field needs to be in ISO 8601"))
      }

    }

    def validateArticle(article: Article, allowUnknownLanguage: Boolean): Try[Article] = {
      val validationErrors = article.content.toSeq.flatMap(c => validateArticleContent(c, allowUnknownLanguage)) ++
        article.introduction.flatMap(i => validateIntroduction(i, allowUnknownLanguage)) ++
        article.metaDescription.flatMap(m => validateMetaDescription(m, allowUnknownLanguage)) ++
        validateTitles(article.title, allowUnknownLanguage) ++
        article.copyright.map(x => validateCopyright(x)).toSeq.flatten ++
        validateTags(article.tags, allowUnknownLanguage) ++
        article.requiredLibraries.flatMap(validateRequiredLibrary) ++
        article.metaImage.flatMap(validateMetaImage) ++
        article.visualElement.flatMap(v => validateVisualElement(v, allowUnknownLanguage))

      if (validationErrors.isEmpty) {
        Success(article)
      } else {
        Failure(new ValidationException(errors = validationErrors))
      }

    }

    def validateArticleApiArticle(id: Long, importValidate: Boolean): Try[ContentId] = {
      draftRepository.withId(id) match {
        case None => Failure(NotFoundException(s"Article with id $id does not exist"))
        case Some(art) =>
          articleApiClient
            .validateArticle(converterService.toArticleApiArticle(art), importValidate)
            .map(_ => ContentId(id))
      }
    }

    def validateArticleApiArticle(id: Long,
                                  updatedArticle: UpdatedArticle,
                                  importValidate: Boolean,
                                  user: UserInfo): Try[ContentId] = {
      draftRepository.withId(id) match {
        case None => Failure(NotFoundException(s"Article with id $id does not exist"))
        case Some(existing) =>
          converterService
            .toDomainArticle(existing, updatedArticle, false, user, None, None)
            .map(converterService.toArticleApiArticle)
            .flatMap(articleApiClient.validateArticle(_, importValidate))
            .map(_ => ContentId(id))
      }
    }

    private def validateArticleContent(content: ArticleContent,
                                       allowUnknownLanguage: Boolean): Seq[ValidationMessage] = {
      HtmlValidator.validate("content", content.content).toList ++
        rootElementContainsOnlySectionBlocks("content.content", content.content) ++
        validateLanguage("content.language", content.language, allowUnknownLanguage)
    }

    def rootElementContainsOnlySectionBlocks(field: String, html: String): Option[ValidationMessage] = {
      val legalTopLevelTag = "section"
      val topLevelTags = HtmlTagRules.stringToJsoupDocument(html).children().asScala.map(_.tagName())

      topLevelTags.forall(_ == legalTopLevelTag) match {
        case true => None
        case false =>
          val illegalTags = topLevelTags.filterNot(_ == legalTopLevelTag).mkString(",")
          Some(
            ValidationMessage(
              field,
              s"An article must consist of one or more <section> blocks. Illegal tag(s) are $illegalTags "))
      }
    }

    private def validateVisualElement(content: VisualElement, allowUnknownLanguage: Boolean): Seq[ValidationMessage] = {
      HtmlValidator
        .validate("visualElement", content.resource, requiredToOptional = Map("image" -> Seq("data-caption")))
        .toList ++
        validateLanguage("language", content.language, allowUnknownLanguage)
    }

    private def validateIntroduction(content: ArticleIntroduction,
                                     allowUnknownLanguage: Boolean): Seq[ValidationMessage] = {
      NoHtmlValidator.validate("introduction", content.introduction).toList ++
        validateLanguage("language", content.language, allowUnknownLanguage)
    }

    private def validateMetaDescription(content: ArticleMetaDescription,
                                        allowUnknownLanguage: Boolean): Seq[ValidationMessage] = {
      NoHtmlValidator.validate("metaDescription", content.content).toList ++
        validateLanguage("language", content.language, allowUnknownLanguage)
    }

    private def validateTitles(titles: Set[ArticleTitle], allowUnknownLanguage: Boolean): Seq[ValidationMessage] = {
      if (titles.isEmpty)
        Seq(
          ValidationMessage(
            "title",
            "An article must contain at least one title. Perhaps you tried to delete the only title in the article?"))
      else
        titles.toSeq.flatMap(t => validateTitle(t.title, t.language, allowUnknownLanguage))
    }

    private def validateTitle(title: String,
                              language: String,
                              allowUnknownLanguage: Boolean): Seq[ValidationMessage] = {
      NoHtmlValidator.validate(s"title.$language", title).toList ++
        validateLanguage("language", language, allowUnknownLanguage) ++
        validateLength(s"title.$language", title, 256) ++
        validateMinimumLength(s"title.$language", title, 1)
    }

    private def validateAgreementCopyright(copyright: Copyright): Seq[ValidationMessage] = {
      val agreementMessage = copyright.agreementId
        .map(_ => ValidationMessage("copyright.agreementId", "Agreement copyrights cant contain agreements"))
        .toSeq
      agreementMessage ++ validateCopyright(copyright)
    }

    private def validateCopyright(copyright: Copyright): Seq[ValidationMessage] = {
      val licenseMessage = copyright.license.map(validateLicense).toSeq.flatten
      val contributorsMessages = copyright.creators.flatMap(validateAuthor) ++ copyright.processors.flatMap(
        validateAuthor) ++ copyright.rightsholders.flatMap(validateAuthor)
      val originMessage =
        copyright.origin.map(origin => NoHtmlValidator.validate("copyright.origin", origin)).toSeq.flatten

      licenseMessage ++ contributorsMessages ++ originMessage
    }

    private def validateLicense(license: String): Seq[ValidationMessage] = {
      getLicense(license) match {
        case None => Seq(ValidationMessage("license.license", s"$license is not a valid license"))
        case _    => Seq()
      }
    }

    private def validateAuthor(author: Author): Seq[ValidationMessage] = {
      NoHtmlValidator.validate("author.type", author.`type`).toList ++
        NoHtmlValidator.validate("author.name", author.name).toList
    }

    private def validateTags(tags: Set[ArticleTag], allowUnknownLanguage: Boolean): Seq[ValidationMessage] = {
      tags.toSeq.flatMap(tagList => {
        tagList.tags.flatMap(NoHtmlValidator.validate("tags", _)).toList :::
          validateLanguage("language", tagList.language, allowUnknownLanguage).toList
      })
    }

    private def validateRequiredLibrary(requiredLibrary: RequiredLibrary): Option[ValidationMessage] = {
      val permittedLibraries = Seq(NDLABrightcoveVideoScriptUrl, H5PResizerScriptUrl) ++ NRKVideoScriptUrl
      permittedLibraries.contains(requiredLibrary.url) match {
        case false =>
          Some(ValidationMessage(
            "requiredLibraries.url",
            s"${requiredLibrary.url} is not a permitted script. Allowed scripts are: ${permittedLibraries.mkString(",")}"))
        case true => None
      }
    }

    private def validateMetaImage(metaImage: ArticleMetaImage): Seq[ValidationMessage] =
      (validateMetaImageId(metaImage.imageId) ++ validateMetaImageAltText(metaImage.altText)).toSeq

    private def validateMetaImageAltText(altText: String): Seq[ValidationMessage] =
      NoHtmlValidator.validate("metaImage.alt", altText)

    private def validateMetaImageId(id: String) = {
      def isAllDigits(x: String) = x forall Character.isDigit
      isAllDigits(id) match {
        case true if id.size > 0 => None
        case _                   => Some(ValidationMessage("metaImageId", "Meta image ID must be a number"))
      }
    }

    private def validateLanguage(fieldPath: String,
                                 languageCode: String,
                                 allowUnknownLanguage: Boolean): Option[ValidationMessage] = {
      languageCode.nonEmpty && languageCodeSupported6391(languageCode, allowUnknownLanguage) match {
        case true  => None
        case false => Some(ValidationMessage(fieldPath, s"Language '$languageCode' is not a supported value."))
      }
    }

    private def validateLength(fieldPath: String, content: String, maxLength: Int): Option[ValidationMessage] = {
      if (content.length > maxLength)
        Some(ValidationMessage(fieldPath, s"This field exceeds the maximum permitted length of $maxLength characters"))
      else
        None
    }

    private def validateMinimumLength(fieldPath: String, content: String, minLength: Int): Option[ValidationMessage] =
      if (content.trim.length < minLength)
        Some(
          ValidationMessage(fieldPath,
                            s"This field does not meet the minimum length requirement of $minLength characters"))
      else
        None

    private def languageCodeSupported6391(languageCode: String, allowUnknownLanguage: Boolean): Boolean = {
      val languageCodes = get6391CodeFor6392CodeMappings.values.toSeq ++ (if (allowUnknownLanguage) Seq("unknown")
                                                                          else Seq.empty)
      languageCodes.contains(languageCode)
    }

  }
}
