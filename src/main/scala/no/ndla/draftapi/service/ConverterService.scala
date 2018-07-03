/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.text.{DateFormat, SimpleDateFormat}
import java.util.Date

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties.externalApiUrls
import no.ndla.draftapi.auth.User
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.{NewAgreement, NotFoundException}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.model.domain.{ArticleStatus, ArticleType, LanguageField}
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.mapping.License.getLicense
import no.ndla.validation._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scala.collection.JavaConverters._
import scala.util.control.Exception.allCatch
import scala.util.{Failure, Success, Try}

trait ConverterService {
  this: Clock with DraftRepository with User with ArticleApiClient =>
  val converterService: ConverterService

  class ConverterService extends LazyLogging {

    def toDomainArticle(newArticle: api.NewArticle,
                        externalId: List[String],
                        oldNdlaCreatedDate: Option[Date],
                        oldNdlaUpdatedDate: Option[Date]): Try[domain.Article] = {
      articleApiClient.allocateArticleId(List.empty, Seq.empty) match {
        case Failure(ex) =>
          Failure(ex)
        case Success(id) =>
          val domainTitles = Seq(domain.ArticleTitle(newArticle.title, newArticle.language))
          val domainContent = newArticle.content
            .map(content => domain.ArticleContent(removeUnknownEmbedTagAttributes(content), newArticle.language))
            .toSeq

          val status = externalId match {
            case Nil          => Set(CREATED)
            case nonEmptyList => Set(CREATED, IMPORTED)
          }

          val oldCreatedDate = oldNdlaCreatedDate.map(date => new DateTime(date).toDate)
          val oldUpdatedDate = oldNdlaUpdatedDate.map(date => new DateTime(date).toDate)

          Success(
            domain.Article(
              id = Some(id),
              revision = None,
              status,
              title = domainTitles,
              content = domainContent,
              copyright = newArticle.copyright.map(toDomainCopyright),
              tags = toDomainTag(newArticle.tags, newArticle.language),
              requiredLibraries = newArticle.requiredLibraries.map(toDomainRequiredLibraries),
              visualElement =
                newArticle.visualElement.map(visual => toDomainVisualElement(visual, newArticle.language)).toSeq,
              introduction =
                newArticle.introduction.map(intro => toDomainIntroduction(intro, newArticle.language)).toSeq,
              metaDescription =
                newArticle.metaDescription.map(meta => toDomainMetaDescription(meta, newArticle.language)).toSeq,
              metaImage = newArticle.metaImageId.map(meta => toDomainMetaImage(meta, newArticle.language)).toSeq,
              created = oldCreatedDate.getOrElse(clock.now()),
              updated = oldUpdatedDate.getOrElse(clock.now()),
              updatedBy = authUser.userOrClientId(),
              articleType = ArticleType.valueOfOrError(newArticle.articleType),
              newArticle.notes
            ))
      }
    }

    def toDomainAgreement(newAgreement: NewAgreement): domain.Agreement = {
      domain.Agreement(
        id = None,
        title = newAgreement.title,
        content = newAgreement.content,
        copyright = toDomainCopyright(newAgreement.copyright),
        created = clock.now(),
        updated = clock.now(),
        updatedBy = authUser.userOrClientId()
      )
    }

    def toDomainTitle(articleTitle: api.ArticleTitle): domain.ArticleTitle =
      domain.ArticleTitle(articleTitle.title, articleTitle.language)

    def toDomainContent(articleContent: api.ArticleContent): domain.ArticleContent = {
      domain.ArticleContent(removeUnknownEmbedTagAttributes(articleContent.content), articleContent.language)
    }

    def toDomainTag(tag: api.ArticleTag): domain.ArticleTag = domain.ArticleTag(tag.tags, tag.language)

    def toDomainTag(tag: Seq[String], language: String): Seq[domain.ArticleTag] = Seq(domain.ArticleTag(tag, language))

    def toDomainVisualElement(visual: api.VisualElement): domain.VisualElement = {
      domain.VisualElement(removeUnknownEmbedTagAttributes(visual.visualElement), visual.language)
    }

    def toDomainVisualElement(visual: String, language: String): domain.VisualElement =
      domain.VisualElement(removeUnknownEmbedTagAttributes(visual), language)

    def toDomainIntroduction(intro: api.ArticleIntroduction): domain.ArticleIntroduction = {
      domain.ArticleIntroduction(intro.introduction, intro.language)
    }

    def toDomainIntroduction(intro: String, language: String): domain.ArticleIntroduction =
      domain.ArticleIntroduction(intro, language)

    def toDomainMetaDescription(meta: api.ArticleMetaDescription): domain.ArticleMetaDescription = {
      domain.ArticleMetaDescription(meta.metaDescription, meta.language)
    }

    def toDomainMetaDescription(meta: String, language: String): domain.ArticleMetaDescription =
      domain.ArticleMetaDescription(meta, language)

    def toDomainMetaImage(imageId: String, language: String): domain.ArticleMetaImage =
      domain.ArticleMetaImage(imageId, language)

    def toDomainCopyright(newCopyright: api.NewAgreementCopyright): domain.Copyright = {
      val parser = ISODateTimeFormat.dateOptionalTimeParser()
      val validFrom = newCopyright.validFrom.flatMap(date => allCatch.opt(parser.parseDateTime(date).toDate))
      val validTo = newCopyright.validTo.flatMap(date => allCatch.opt(parser.parseDateTime(date).toDate))

      val apiCopyright = api.Copyright(
        newCopyright.license,
        newCopyright.origin,
        newCopyright.creators,
        newCopyright.processors,
        newCopyright.rightsholders,
        newCopyright.agreementId,
        validFrom,
        validTo
      )
      toDomainCopyright(apiCopyright)
    }

    def toDomainCopyright(copyright: api.Copyright): domain.Copyright = {
      domain.Copyright(
        copyright.license.map(_.license),
        copyright.origin,
        copyright.creators.map(toDomainAuthor),
        copyright.processors.map(toDomainAuthor),
        copyright.rightsholders.map(toDomainAuthor),
        copyright.agreementId,
        copyright.validFrom,
        copyright.validTo
      )
    }

    def toDomainAuthor(author: api.Author): domain.Author = domain.Author(author.`type`, author.name)

    def toDomainRequiredLibraries(requiredLibs: api.RequiredLibrary): domain.RequiredLibrary = {
      domain.RequiredLibrary(requiredLibs.mediaType, requiredLibs.name, requiredLibs.url)
    }

    private def getLinkToOldNdla(id: Long): Option[String] =
      draftRepository.getExternalIdsFromId(id).map(createLinkToOldNdla).headOption

    private def removeUnknownEmbedTagAttributes(html: String): String = {
      val document = HtmlTagRules.stringToJsoupDocument(html)
      document
        .select("embed")
        .asScala
        .map(el => {
          ResourceType
            .valueOf(el.attr(TagAttributes.DataResource.toString))
            .map(EmbedTagRules.attributesForResourceType)
            .map(knownAttributes => HtmlTagRules.removeIllegalAttributes(el, knownAttributes.all.map(_.toString)))
        })

      HtmlTagRules.jsoupDocumentToString(document)
    }

    def toApiArticle(article: domain.Article, language: String, fallback: Boolean = false): Try[api.Article] = {
      val supportedLanguages = getSupportedLanguages(
        Seq(article.title,
            article.visualElement,
            article.introduction,
            article.metaDescription,
            article.tags,
            article.content)
      )
      val isLanguageNeutral = supportedLanguages.contains(UnknownLanguage) && supportedLanguages.length == 1

      if (supportedLanguages.contains(language) || language == AllLanguages || isLanguageNeutral || fallback) {
        val metaDescription =
          findByLanguageOrBestEffort(article.metaDescription, language).map(toApiArticleMetaDescription)
        val tags = findByLanguageOrBestEffort(article.tags, language).map(toApiArticleTag)
        val title = findByLanguageOrBestEffort(article.title, language).map(toApiArticleTitle)
        val introduction = findByLanguageOrBestEffort(article.introduction, language).map(toApiArticleIntroduction)
        val visualElement = findByLanguageOrBestEffort(article.visualElement, language).map(toApiVisualElement)
        val articleContent = findByLanguageOrBestEffort(article.content, language).map(toApiArticleContent)
        val metaImage = findByLanguageOrBestEffort(article.metaImage, language).map(toApiArticleMetaImage)

        Success(
          api.Article(
            article.id.get,
            article.id.flatMap(getLinkToOldNdla),
            article.revision.get,
            article.status.map(_.toString),
            title,
            articleContent,
            article.copyright.map(toApiCopyright),
            tags,
            article.requiredLibraries.map(toApiRequiredLibrary),
            visualElement,
            introduction,
            metaDescription,
            metaImage,
            article.created,
            article.updated,
            article.updatedBy,
            article.articleType.toString,
            supportedLanguages,
            article.notes
          ))
      } else {
        Failure(
          NotFoundException(s"The article with id ${article.id.get} and language $language was not found",
                            supportedLanguages))
      }
    }

    def toApiAgreement(agreement: domain.Agreement): api.Agreement = {
      api.Agreement(
        id = agreement.id.get,
        title = agreement.title,
        content = agreement.content,
        copyright = toApiCopyright(agreement.copyright),
        created = agreement.created,
        updated = agreement.updated,
        updatedBy = agreement.updatedBy
      )
    }

    def toDomainStatus(status: api.ArticleStatus): Try[Set[ArticleStatus.Value]] = {
      val (validStatuses, invalidStatuses) = status.status.map(ArticleStatus.valueOfOrError).partition(_.isSuccess)
      if (invalidStatuses.nonEmpty) {
        val errors = invalidStatuses.flatMap {
          case Failure(ex: ValidationException) => ex.errors
          case Failure(ex)                      => Set(ValidationMessage("status", ex.getMessage))
          case Success(_)                       => Set()
        }
        Failure(new ValidationException(errors = errors.toSeq))
      } else
        Success(validStatuses.map(_.get))
    }

    def toApiStatus(status: Set[ArticleStatus.Value]): api.ArticleStatus = api.ArticleStatus(status.map(_.toString))

    def toApiArticleTitle(title: domain.ArticleTitle): api.ArticleTitle = api.ArticleTitle(title.title, title.language)

    def toApiArticleContent(content: domain.ArticleContent): api.ArticleContent =
      api.ArticleContent(content.content, content.language)

    def toApiArticleMetaImage(metaImage: domain.ArticleMetaImage): api.ArticleMetaImage = {
      api.ArticleMetaImage(s"${externalApiUrls("raw-image")}/${metaImage.imageId}", metaImage.language)
    }

    def toApiCopyright(copyright: domain.Copyright): api.Copyright = {
      api.Copyright(
        copyright.license.map(toApiLicense),
        copyright.origin,
        copyright.creators.map(toApiAuthor),
        copyright.processors.map(toApiAuthor),
        copyright.rightsholders.map(toApiAuthor),
        copyright.agreementId,
        copyright.validFrom,
        copyright.validTo
      )
    }

    def toApiLicense(shortLicense: String): api.License = {
      getLicense(shortLicense)
        .map(l => api.License(l.license, Option(l.description), l.url))
        .getOrElse(api.License("unknown", None, None))
    }

    def toApiAuthor(author: domain.Author): api.Author = api.Author(author.`type`, author.name)

    def toApiArticleTag(tag: domain.ArticleTag): api.ArticleTag = api.ArticleTag(tag.tags, tag.language)

    def toApiRequiredLibrary(required: domain.RequiredLibrary): api.RequiredLibrary = {
      api.RequiredLibrary(required.mediaType, required.name, required.url)
    }

    def toApiVisualElement(visual: domain.VisualElement): api.VisualElement =
      api.VisualElement(visual.resource, visual.language)

    def toApiArticleIntroduction(intro: domain.ArticleIntroduction): api.ArticleIntroduction = {
      api.ArticleIntroduction(intro.introduction, intro.language)
    }

    def toApiArticleMetaDescription(metaDescription: domain.ArticleMetaDescription): api.ArticleMetaDescription = {
      api.ArticleMetaDescription(metaDescription.content, metaDescription.language)
    }

    def createLinkToOldNdla(nodeId: String): String = s"//red.ndla.no/node/$nodeId"

    def toApiConcept(concept: domain.Concept, language: String): api.Concept = {
      val title = findByLanguageOrBestEffort(concept.title, language)
        .map(toApiConceptTitle)
        .getOrElse(api.ConceptTitle("", DefaultLanguage))
      val content = findByLanguageOrBestEffort(concept.content, language)
        .map(toApiConceptContent)
        .getOrElse(api.ConceptContent("", DefaultLanguage))

      api.Concept(
        concept.id.get,
        Some(title),
        Some(content),
        concept.copyright.map(toApiCopyright),
        concept.created,
        concept.updated,
        concept.supportedLanguages
      )
    }

    def toApiConceptTitle(title: domain.ConceptTitle): api.ConceptTitle = api.ConceptTitle(title.title, title.language)

    def toApiConceptContent(title: domain.ConceptContent): api.ConceptContent =
      api.ConceptContent(title.content, title.language)

    def toArticleApiCopyright(copyright: domain.Copyright): api.ArticleApiCopyright = {
      def toArticleApiAuthor(author: domain.Author): api.ArticleApiAuthor =
        api.ArticleApiAuthor(author.`type`, author.name)
      api.ArticleApiCopyright(
        copyright.license.getOrElse(""),
        copyright.origin.getOrElse(""),
        copyright.creators.map(toArticleApiAuthor),
        copyright.processors.map(toArticleApiAuthor),
        copyright.rightsholders.map(toArticleApiAuthor),
        copyright.agreementId,
        copyright.validFrom,
        copyright.validTo
      )
    }

    def toArticleApiArticle(article: domain.Article): api.ArticleApiArticle = {
      api.ArticleApiArticle(
        revision = article.revision,
        title = article.title.map(t => api.ArticleApiTitle(t.title, t.language)),
        content = article.content.map(c => api.ArticleApiContent(c.content, c.language)),
        copyright = article.copyright.map(toArticleApiCopyright),
        tags = article.tags.map(t => api.ArticleApiTag(t.tags, t.language)),
        requiredLibraries =
          article.requiredLibraries.map(r => api.ArticleApiRequiredLibrary(r.mediaType, r.name, r.url)),
        visualElement = article.visualElement.map(v => api.ArticleApiVisualElement(v.resource, v.language)),
        introduction = article.introduction.map(i => api.ArticleApiIntroduction(i.introduction, i.language)),
        metaDescription = article.metaDescription.map(m => api.ArticleApiMetaDescription(m.content, m.language)),
        metaImage = article.metaImage.map(m => api.ArticleApiMetaImage(m.imageId, m.language)),
        created = article.created,
        updated = article.updated,
        updatedBy = article.updatedBy,
        articleType = article.articleType.toString
      )
    }

    def toArticleApiConcept(article: domain.Concept): api.ArticleApiConcept = {
      api.ArticleApiConcept(
        title = article.title.map(t => api.ArticleApiConceptTitle(t.title, t.language)),
        content = article.content.map(c => api.ArticleApiConceptContent(c.content, c.language)),
        copyright = article.copyright.map(toArticleApiCopyright),
        created = article.created,
        updated = article.updated
      )
    }

    def toDomainConcept(concept: api.NewConcept): Try[domain.Concept] = {
      articleApiClient.allocateConceptId(List.empty) match {
        case Failure(ex) => Failure(ex)
        case Success(id) =>
          Success(
            domain.Concept(
              Some(id),
              Seq(domain.ConceptTitle(concept.title, concept.language)),
              concept.content
                .map(content => Seq(domain.ConceptContent(content, concept.language)))
                .getOrElse(Seq.empty),
              concept.copyright.map(toDomainCopyright),
              clock.now(),
              clock.now()
            ))
      }
    }

    private def languageFieldIsDefined(article: api.UpdatedArticle): Boolean = {
      val langFields: Seq[Option[_]] = Seq(article.title,
                                           article.content,
                                           article.tags,
                                           article.introduction,
                                           article.metaDescription,
                                           article.metaImageId,
                                           article.visualElement)

      langFields.foldRight(false)((curr, res) => res || curr.isDefined)
    }

    def toDomainArticle(toMergeInto: domain.Article,
                        article: api.UpdatedArticle,
                        isImported: Boolean,
                        oldNdlaCreatedDate: Option[Date],
                        oldNdlaUpdatedDate: Option[Date]): Try[domain.Article] = {
      val status = toMergeInto.status.filterNot(s => s == CREATED || s == PUBLISHED) + (if (!isImported) DRAFT
                                                                                        else IMPORTED)
      val createdDate = if (isImported) oldNdlaCreatedDate.getOrElse(toMergeInto.created) else toMergeInto.created
      val updatedDate = if (isImported) oldNdlaUpdatedDate.getOrElse(clock.now()) else clock.now()
      val partiallyConverted = toMergeInto.copy(
        status = status,
        revision = Option(article.revision),
        copyright = article.copyright.map(toDomainCopyright).orElse(toMergeInto.copyright),
        requiredLibraries = article.requiredLibraries.map(y => y.map(x => toDomainRequiredLibraries(x))).toSeq.flatten,
        created = createdDate,
        updated = updatedDate,
        updatedBy = authUser.userOrClientId(),
        articleType = article.articleType.map(ArticleType.valueOfOrError).getOrElse(toMergeInto.articleType),
        notes = article.notes.getOrElse(toMergeInto.notes)
      )

      article.language match {
        case None if languageFieldIsDefined(article) =>
          val error = ValidationMessage("language", "This field must be specified when updating language fields")
          Failure(new ValidationException(errors = Seq(error)))
        case None => Success(partiallyConverted)
        case Some(lang) =>
          Success(
            partiallyConverted.copy(
              title = mergeLanguageFields(toMergeInto.title,
                                          article.title.toSeq.map(t => toDomainTitle(api.ArticleTitle(t, lang)))),
              content = mergeLanguageFields(toMergeInto.content,
                                            article.content.toSeq.map(c =>
                                              toDomainContent(api.ArticleContent(c, lang)))),
              tags = article.tags
                .map(tags => mergeLanguageFields(toMergeInto.tags, toDomainTag(tags, lang)))
                .getOrElse(toMergeInto.tags),
              visualElement = mergeLanguageFields(toMergeInto.visualElement,
                                                  article.visualElement.map(c => toDomainVisualElement(c, lang)).toSeq),
              introduction = mergeLanguageFields(toMergeInto.introduction,
                                                 article.introduction.map(i => toDomainIntroduction(i, lang)).toSeq),
              metaDescription =
                mergeLanguageFields(toMergeInto.metaDescription,
                                    article.metaDescription.map(m => toDomainMetaDescription(m, lang)).toSeq),
              metaImage =
                mergeLanguageFields(toMergeInto.metaImage, article.metaImageId.map(toDomainMetaImage(_, lang)).toSeq)
            ))
      }
    }

    def toDomainArticle(id: Long,
                        article: api.UpdatedArticle,
                        isImported: Boolean,
                        oldNdlaCreatedDate: Option[Date],
                        oldNdlaUpdatedDate: Option[Date]): Try[domain.Article] = {
      val createdDate = oldNdlaCreatedDate.getOrElse(clock.now())
      val updatedDate = oldNdlaUpdatedDate.getOrElse(clock.now())

      article.language match {
        case None =>
          val error = ValidationMessage("language", "This field must be specified when updating language fields")
          Failure(new ValidationException(errors = Seq(error)))
        case Some(lang) =>
          val status = if (isImported) Set(ArticleStatus.IMPORTED) else Set(ArticleStatus.CREATED)
          Success(
            domain.Article(
              id = Some(id),
              revision = Some(1),
              status = status,
              title = article.title.map(t => domain.ArticleTitle(t, lang)).toSeq,
              content = article.content.map(c => domain.ArticleContent(c, lang)).toSeq,
              copyright = article.copyright.map(toDomainCopyright),
              tags = article.tags.toSeq.map(tags => domain.ArticleTag(tags, lang)),
              requiredLibraries = article.requiredLibraries.map(_.map(toDomainRequiredLibraries)).toSeq.flatten,
              visualElement = article.visualElement.map(v => toDomainVisualElement(v, lang)).toSeq,
              introduction = article.introduction.map(i => toDomainIntroduction(i, lang)).toSeq,
              metaDescription = article.metaDescription.map(m => toDomainMetaDescription(m, lang)).toSeq,
              metaImage = article.metaImageId.map(m => toDomainMetaImage(m, lang)).toSeq,
              created = createdDate,
              updated = updatedDate,
              authUser.userOrClientId(),
              articleType = article.articleType.map(ArticleType.valueOfOrError).getOrElse(ArticleType.Standard),
              article.notes.getOrElse(Seq.empty)
            ))
      }
    }

    def toDomainConcept(toMergeInto: domain.Concept, updateConcept: api.UpdatedConcept): domain.Concept = {
      val domainTitle = updateConcept.title.map(t => domain.ConceptTitle(t, updateConcept.language)).toSeq
      val domainContent = updateConcept.content.map(c => domain.ConceptContent(c, updateConcept.language)).toSeq

      toMergeInto.copy(
        title = mergeLanguageFields(toMergeInto.title, domainTitle),
        content = mergeLanguageFields(toMergeInto.content, domainContent),
        copyright = updateConcept.copyright.map(toDomainCopyright).orElse(toMergeInto.copyright),
        created = toMergeInto.created,
        updated = clock.now()
      )
    }

    def toDomainConcept(id: Long, article: api.UpdatedConcept): domain.Concept = {
      val lang = article.language

      domain.Concept(
        id = Some(id),
        title = article.title.map(t => domain.ConceptTitle(t, lang)).toSeq,
        content = article.content.map(c => domain.ConceptContent(c, lang)).toSeq,
        copyright = article.copyright.map(toDomainCopyright),
        created = clock.now(),
        updated = clock.now()
      )
    }

    private[service] def mergeLanguageFields[A <: LanguageField[_]](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }

  }

}
