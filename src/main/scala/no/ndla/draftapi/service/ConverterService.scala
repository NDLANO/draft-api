/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.util.Date

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties.externalApiUrls
import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.{NewAgreement, NotFoundException}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.model.domain.{ArticleStatus, _}
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
  this: Clock with DraftRepository with ArticleApiClient with StateTransitionRules =>
  val converterService: ConverterService

  class ConverterService extends LazyLogging {

    def toDomainArticle(newArticle: api.NewArticle,
                        externalIds: List[String],
                        user: UserInfo,
                        oldNdlaCreatedDate: Option[Date],
                        oldNdlaUpdatedDate: Option[Date]): Try[domain.Article] = {
      draftRepository.newArticleId() match {
        case Failure(ex) => Failure(ex)
        case Success(id) =>
          val domainTitles = Seq(domain.ArticleTitle(newArticle.title, newArticle.language))
          val domainContent = newArticle.content
            .map(content => domain.ArticleContent(removeUnknownEmbedTagAttributes(content), newArticle.language))
            .toSeq

          val status = externalIds match {
            case Nil => domain.Status(DRAFT, Set.empty)
            case _   => domain.Status(DRAFT, Set(IMPORTED))
          }

          val oldCreatedDate = oldNdlaCreatedDate.map(date => new DateTime(date).toDate)
          val oldUpdatedDate = oldNdlaUpdatedDate.map(date => new DateTime(date).toDate)

          newNotes(newArticle.notes, user, status).map(
            notes =>
              domain.Article(
                id = Some(id),
                revision = None,
                status,
                title = domainTitles,
                content = domainContent,
                copyright = newArticle.copyright.map(toDomainCopyright),
                tags = toDomainTag(newArticle.tags, newArticle.language).toSeq,
                requiredLibraries = newArticle.requiredLibraries.map(toDomainRequiredLibraries),
                visualElement =
                  newArticle.visualElement.map(visual => toDomainVisualElement(visual, newArticle.language)).toSeq,
                introduction =
                  newArticle.introduction.map(intro => toDomainIntroduction(intro, newArticle.language)).toSeq,
                metaDescription =
                  newArticle.metaDescription.map(meta => toDomainMetaDescription(meta, newArticle.language)).toSeq,
                metaImage = newArticle.metaImage.map(meta => toDomainMetaImage(meta, newArticle.language)).toSeq,
                created = oldCreatedDate.getOrElse(clock.now()),
                updated = oldUpdatedDate.getOrElse(clock.now()),
                updatedBy = user.id,
                published = oldUpdatedDate.getOrElse(newArticle.published.getOrElse(clock.now())), // If import use old updated. Else use new published or now
                articleType = ArticleType.valueOfOrError(newArticle.articleType),
                notes
            ))
      }
    }

    private[service] def newNotes(notes: Seq[String], user: UserInfo, status: Status): Try[Seq[EditorNote]] = {
      notes match {
        case Nil                  => Success(Seq.empty)
        case l if !l.contains("") => Success(l.map(domain.EditorNote(_, user.id, status, new Date())))
        case _ =>
          Failure(
            new ValidationException(errors = Seq(ValidationMessage("notes", "A note can not be an empty string"))))
      }
    }

    def toDomainAgreement(newAgreement: NewAgreement, user: UserInfo): domain.Agreement = {
      domain.Agreement(
        id = None,
        title = newAgreement.title,
        content = newAgreement.content,
        copyright = toDomainCopyright(newAgreement.copyright),
        created = clock.now(),
        updated = clock.now(),
        updatedBy = user.id
      )
    }

    def toDomainTitle(articleTitle: api.ArticleTitle): domain.ArticleTitle =
      domain.ArticleTitle(articleTitle.title, articleTitle.language)

    def toDomainContent(articleContent: api.ArticleContent): domain.ArticleContent = {
      domain.ArticleContent(removeUnknownEmbedTagAttributes(articleContent.content), articleContent.language)
    }

    def toDomainTag(tag: api.ArticleTag): domain.ArticleTag = domain.ArticleTag(tag.tags, tag.language)

    def toDomainTag(tag: Seq[String], language: String): Option[domain.ArticleTag] =
      if (tag.nonEmpty) Some(domain.ArticleTag(tag, language)) else None

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

    def toDomainMetaImage(metaImage: api.NewArticleMetaImage, language: String): domain.ArticleMetaImage =
      domain.ArticleMetaImage(metaImage.id, metaImage.alt, language)

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

    def updateStatus(status: ArticleStatus.Value,
                     article: domain.Article,
                     user: UserInfo,
                     isImported: Boolean): IO[Try[domain.Article]] =
      StateTransitionRules.doTransition(article, status, user, isImported)

    def toApiArticle(article: domain.Article, language: String, fallback: Boolean = false): Try[api.Article] = {
      val isLanguageNeutral = article.supportedLanguages.contains(UnknownLanguage) && article.supportedLanguages.length == 1

      if (article.supportedLanguages.contains(language) || language == AllLanguages || isLanguageNeutral || fallback) {
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
            toApiStatus(article.status),
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
            article.published,
            article.articleType.toString,
            article.supportedLanguages,
            article.notes.map(toApiEditorNote)
          ))
      } else {
        Failure(
          NotFoundException(s"The article with id ${article.id.get} and language $language was not found",
                            article.supportedLanguages))
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

    def toDomainStatus(status: api.Status): Try[domain.Status] = {
      val newCurrent = ArticleStatus.valueOfOrError(status.current)
      val newOther = status.other.map(ArticleStatus.valueOfOrError)
      val (newOtherValids, newOtherInvalids) = newOther.partition(_.isSuccess)

      (newCurrent, newOtherInvalids, newOtherValids) match {
        case (Failure(_), _, _) => Failure(new ValidationException(s"Status ${status.current} is invalid", Seq()))
        case (_, invalidOthers, _) if invalidOthers.nonEmpty =>
          val messages = invalidOthers.map(_.failed.get).map(x => ValidationMessage("status.other", x.getMessage))
          Failure(new ValidationException(s"One or more status(es) are invalid", messages))
        case (Success(current), _, others) if others.forall(_.isSuccess) =>
          Success(domain.Status(current, others.map(_.get).toSet))
      }

    }

    def toApiEditorNote(note: domain.EditorNote): api.EditorNote =
      api.EditorNote(note.note, note.user, toApiStatus(note.status), note.timestamp)

    def toApiStatus(status: domain.Status): api.Status =
      api.Status(status.current.toString, status.other.map(_.toString).toSeq)

    def toApiArticleTitle(title: domain.ArticleTitle): api.ArticleTitle = api.ArticleTitle(title.title, title.language)

    def toApiArticleContent(content: domain.ArticleContent): api.ArticleContent =
      api.ArticleContent(content.content, content.language)

    def toApiArticleMetaImage(metaImage: domain.ArticleMetaImage): api.ArticleMetaImage = {
      api.ArticleMetaImage(s"${externalApiUrls("raw-image")}/${metaImage.imageId}",
                           metaImage.altText,
                           metaImage.language)
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
        .map(l => api.License(l.license.toString, Option(l.description), l.url))
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
        metaImage = article.metaImage.map(m => api.ArticleApiMetaImage(m.imageId, m.altText, m.language)),
        created = article.created,
        updated = article.updated,
        updatedBy = article.updatedBy,
        published = article.published,
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
                                           article.metaImage,
                                           article.visualElement)

      langFields.foldRight(false)((curr, res) => res || curr.isDefined)
    }

    def toDomainArticle(toMergeInto: domain.Article,
                        article: api.UpdatedArticle,
                        isImported: Boolean,
                        user: UserInfo,
                        oldNdlaCreatedDate: Option[Date],
                        oldNdlaUpdatedDate: Option[Date]): Try[domain.Article] = {

      val createdDate = if (isImported) oldNdlaCreatedDate.getOrElse(toMergeInto.created) else toMergeInto.created
      val updatedDate = if (isImported) oldNdlaUpdatedDate.getOrElse(clock.now()) else clock.now()
      val publishedDate = article.published.getOrElse(toMergeInto.published)

      val isNewLanguage = article.language.map(l => !toMergeInto.supportedLanguages.contains(l)).getOrElse(false)
      val newLanguageEditorNote =
        if (isNewLanguage) Seq(s"Ny språkvariant '${article.language.getOrElse("unknown")}' ble lagt til.")
        else Seq.empty

      val newEditorialNotes = article.notes match {
        case Some(n) => newNotes(n ++ newLanguageEditorNote, user, toMergeInto.status)
        case None    => newNotes(newLanguageEditorNote, user, toMergeInto.status)
      }

      newEditorialNotes.map(notes => toMergeInto.notes ++ notes) match {
        case Failure(ex) => Failure(ex)
        case Success(allNotes) =>
          val partiallyConverted = toMergeInto.copy(
            revision = Option(article.revision),
            copyright = article.copyright.map(toDomainCopyright).orElse(toMergeInto.copyright),
            requiredLibraries =
              article.requiredLibraries.map(y => y.map(x => toDomainRequiredLibraries(x))).toSeq.flatten,
            created = createdDate,
            updated = updatedDate,
            published = publishedDate,
            updatedBy = user.id,
            articleType = article.articleType.map(ArticleType.valueOfOrError).getOrElse(toMergeInto.articleType),
            notes = allNotes
          )

          article.language match {
            case None if languageFieldIsDefined(article) =>
              val error = ValidationMessage("language", "This field must be specified when updating language fields")
              Failure(new ValidationException(errors = Seq(error)))
            case None => Success(partiallyConverted)
            case Some(lang) =>
              Success(mergeArticleLanguageFields(partiallyConverted, article, lang))
          }
      }

    }

    private[service] def mergeArticleLanguageFields(toMergeInto: Article,
                                                    updatedArticle: api.UpdatedArticle,
                                                    lang: String): Article = {
      val updatedTitles = updatedArticle.title.toSeq.map(t => toDomainTitle(api.ArticleTitle(t, lang)))
      val updatedContents = updatedArticle.content.toSeq.map(c => toDomainContent(api.ArticleContent(c, lang)))
      val updatedTags = updatedArticle.tags.flatMap(tags => toDomainTag(tags, lang)).toSeq
      val updatedVisualElement = updatedArticle.visualElement.map(c => toDomainVisualElement(c, lang)).toSeq
      val updatedIntroductions = updatedArticle.introduction.map(i => toDomainIntroduction(i, lang)).toSeq
      val updatedMetaDescriptions = updatedArticle.metaDescription.map(m => toDomainMetaDescription(m, lang)).toSeq
      val updatedMetaImage = updatedArticle.metaImage.map(toDomainMetaImage(_, lang)).toSeq

      toMergeInto.copy(
        title = mergeLanguageFields(toMergeInto.title, updatedTitles),
        content = mergeLanguageFields(toMergeInto.content, updatedContents),
        tags = mergeLanguageFields(toMergeInto.tags, updatedTags),
        visualElement = mergeLanguageFields(toMergeInto.visualElement, updatedVisualElement),
        introduction = mergeLanguageFields(toMergeInto.introduction, updatedIntroductions),
        metaDescription = mergeLanguageFields(toMergeInto.metaDescription, updatedMetaDescriptions),
        metaImage = mergeLanguageFields(toMergeInto.metaImage, updatedMetaImage)
      )
    }

    def toDomainArticle(id: Long,
                        article: api.UpdatedArticle,
                        isImported: Boolean,
                        user: UserInfo,
                        oldNdlaCreatedDate: Option[Date],
                        oldNdlaUpdatedDate: Option[Date]): Try[domain.Article] = {
      val createdDate = oldNdlaCreatedDate.getOrElse(clock.now())
      val updatedDate = oldNdlaUpdatedDate.getOrElse(clock.now())

      article.language match {
        case None =>
          val error = ValidationMessage("language", "This field must be specified when updating language fields")
          Failure(new ValidationException(errors = Seq(error)))
        case Some(lang) =>
          val status =
            if (isImported) domain.Status(DRAFT, Set(ArticleStatus.IMPORTED))
            else domain.Status(DRAFT, Set.empty)

          val mergedNotes = article.notes.map(n => newNotes(n, user, status)) match {
            case Some(Failure(ex))    => Failure(ex)
            case Some(Success(notes)) => Success(notes)
            case None                 => Success(Seq.empty)
          }

          mergedNotes.map(
            notes =>
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
                metaImage = article.metaImage.map(m => toDomainMetaImage(m, lang)).toSeq,
                created = createdDate,
                updated = updatedDate,
                published = article.published.getOrElse(clock.now()),
                updatedBy = user.id,
                articleType = article.articleType.map(ArticleType.valueOfOrError).getOrElse(ArticleType.Standard),
                notes = notes
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

    private[service] def mergeLanguageFields[A <: LanguageField](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }

    def stateTransitionsToApi(user: UserInfo): Map[String, Seq[String]] = {
      StateTransitionRules.StateTransitions.groupBy(_.from).map {
        case (from, to) =>
          from.toString -> to
            .filter(t => user.hasRoles(t.requiredRoles))
            .map(_.to.toString)
            .toSeq
      }
    }

  }
}
