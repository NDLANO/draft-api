/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service

import com.google.gson.JsonObject
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.auth.User
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.domain.{ArticleStatus, Language, LanguageField}
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.NewAgreement
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.model.domain.{ArticleStatus, ArticleType}
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.mapping.License.getLicense
import no.ndla.validation._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

trait ConverterService {
  this: Clock with DraftRepository with User with ArticleApiClient =>
  val converterService: ConverterService

  class ConverterService extends LazyLogging {

    def getValueByFieldAndLanguage(hit: JsonObject, fieldPath: String, searchLanguage: String): String = {
      hit.get(fieldPath).getAsJsonObject.entrySet.asScala.to[Seq].find(entr => entr.getKey == searchLanguage) match {
        case Some(element) => element.getValue.getAsString
        case None => ""
      }
    }

    def toDomainArticle(newArticle: api.NewArticle, externalId: Option[String]): Try[domain.Article] = {
      ArticleApiClient.allocateArticleId(None, Seq.empty) match {
        case Failure(ex) =>
          Failure(ex)
        case Success(id) =>
          val domainTitles = Seq(domain.ArticleTitle(newArticle.title, newArticle.language))
          val domainContent = newArticle.content.map(content => domain.ArticleContent(removeUnknownEmbedTagAttributes(content), newArticle.language)).toSeq

          val status = externalId match {
            case Some(_) => Set(CREATED, IMPORTED)
            case None => Set(CREATED)
          }

          Success(domain.Article(
            id = Some(id),
            revision = None,
            status,
            title = domainTitles,
            content = domainContent,
            copyright = newArticle.copyright.map(toDomainCopyright),
            tags = toDomainTag(newArticle.tags, newArticle.language),
            requiredLibraries = newArticle.requiredLibraries.map(toDomainRequiredLibraries),
            visualElement = newArticle.visualElement.map(visual => toDomainVisualElement(visual, newArticle.language)).toSeq,
            introduction = newArticle.introduction.map(intro => toDomainIntroduction(intro, newArticle.language)).toSeq,
            metaDescription = newArticle.metaDescription.map(meta => toDomainMetaDescription(meta, newArticle.language)).toSeq,
            metaImageId = newArticle.metaImageId,
            created = clock.now(),
            updated = clock.now(),
            updatedBy = authUser.userOrClientId(),
            articleType = ArticleType.valueOfOrError(newArticle.articleType)
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

    def toDomainTitle(articleTitle: api.ArticleTitle): domain.ArticleTitle = domain.ArticleTitle(articleTitle.title, articleTitle.language)

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

    def toDomainIntroduction(intro: String, language: String): domain.ArticleIntroduction = domain.ArticleIntroduction(intro, language)

    def toDomainMetaDescription(meta: api.ArticleMetaDescription): domain.ArticleMetaDescription = {
      domain.ArticleMetaDescription(meta.metaDescription, meta.language)
    }

    def toDomainMetaDescription(meta: String, language: String): domain.ArticleMetaDescription = domain.ArticleMetaDescription(meta, language)

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

    private def getLinkToOldNdla(id: Long): Option[String] = draftRepository.getExternalIdFromId(id).map(createLinkToOldNdla)

    private def removeUnknownEmbedTagAttributes(html: String): String = {
      val document = HtmlTagRules.stringToJsoupDocument(html)
      document.select("embed").asScala.map(el => {
        ResourceType.valueOf(el.attr(TagAttributes.DataResource.toString))
          .map(EmbedTagRules.attributesForResourceType)
          .map(knownAttributes => HtmlTagRules.removeIllegalAttributes(el, knownAttributes.all.map(_.toString)))
      })

      HtmlTagRules.jsoupDocumentToString(document)
    }

    def toApiArticle(article: domain.Article, language: String): api.Article = {
      val supportedLanguages = getSupportedLanguages(
        Seq(article.title, article.visualElement, article.introduction, article.metaDescription, article.tags, article.content)
      )

      val meta = findByLanguageOrBestEffort(article.metaDescription, language).map(toApiArticleMetaDescription)
      val tags = findByLanguageOrBestEffort(article.tags, language).map(toApiArticleTag)
      val title = findByLanguageOrBestEffort(article.title, language).map(toApiArticleTitle)
      val introduction = findByLanguageOrBestEffort(article.introduction, language).map(toApiArticleIntroduction)
      val visualElement = findByLanguageOrBestEffort(article.visualElement, language).map(toApiVisualElement)
      val articleContent = findByLanguageOrBestEffort(article.content, language).map(toApiArticleContent)

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
        meta,
        article.created,
        article.updated,
        article.updatedBy,
        article.articleType.toString,
        supportedLanguages
      )
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
          case Failure(ex) => Set(ValidationMessage("status", ex.getMessage))
          case Success(_) => Set()
        }
        Failure(new ValidationException(errors=errors.toSeq))
      } else
        Success(validStatuses.map(_.get))
    }

    def toApiStatus(status: Set[ArticleStatus.Value]): api.ArticleStatus = api.ArticleStatus(status.map(_.toString))

    def toApiArticleTitle(title: domain.ArticleTitle): api.ArticleTitle = api.ArticleTitle(title.title, title.language)

    def toApiArticleContent(content: domain.ArticleContent): api.ArticleContent = api.ArticleContent(content.content, content.language)

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

    def toApiVisualElement(visual: domain.VisualElement): api.VisualElement = api.VisualElement(visual.resource, visual.language)

    def toApiArticleIntroduction(intro: domain.ArticleIntroduction): api.ArticleIntroduction = {
      api.ArticleIntroduction(intro.introduction, intro.language)
    }

    def toApiArticleMetaDescription(metaDescription: domain.ArticleMetaDescription): api.ArticleMetaDescription = {
      api.ArticleMetaDescription(metaDescription.content, metaDescription.language)
    }

    def createLinkToOldNdla(nodeId: String): String = s"//red.ndla.no/node/$nodeId"

    def toApiConcept(concept: domain.Concept, language: String): api.Concept = {
      val title = findByLanguageOrBestEffort(concept.title, language).map(toApiConceptTitle).getOrElse(api.ConceptTitle("", DefaultLanguage))
      val content = findByLanguageOrBestEffort(concept.content, language).map(toApiConceptContent).getOrElse(api.ConceptContent("", DefaultLanguage))

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

    def toApiConceptContent(title: domain.ConceptContent): api.ConceptContent = api.ConceptContent(title.content, title.language)

    def toArticleApiCopyright(copyright: domain.Copyright): api.ArticleApiCopyright = {
      def toArticleApiAuthor(author: domain.Author): api.ArticleApiAuthor = api.ArticleApiAuthor(author.`type`, author.name)
      api.ArticleApiCopyright(copyright.license,
        copyright.origin,
        copyright.creators.map(toArticleApiAuthor),
        copyright.processors.map(toArticleApiAuthor),
        copyright.rightsholders.map(toArticleApiAuthor),
        copyright.agreementId,
        copyright.validFrom,
        copyright.validTo
      )
    }

    def toArticleApiOldCopyright(copyright: domain.Copyright): api.ArticleApiOldCopyright = {
      def toArticleApiAuthor(author: domain.Author): api.ArticleApiAuthor = api.ArticleApiAuthor(author.`type`, author.name)
      val authors = copyright.creators ++ copyright.processors ++ copyright.rightsholders
      api.ArticleApiOldCopyright(copyright.license, copyright.origin.getOrElse(""), authors.map(toArticleApiAuthor))
    }

    def toArticleApiArticle(article: domain.Article): api.ArticleApiArticle = {
      api.ArticleApiArticle(
        revision = article.revision,
        title = article.title.map(t => api.ArticleApiTitle(t.title, t.language)),
        content = article.content.map(c => api.ArticleApiContent(c.content, c.language)),
        copyright = article.copyright.map(toArticleApiOldCopyright),
        tags = article.tags.map(t => api.ArticleApiTag(t.tags, t.language)),
        requiredLibraries = article.requiredLibraries.map(r => api.ArticleApiRequiredLibrary(r.mediaType, r.name, r.url)),
        visualElement = article.visualElement.map(v => api.ArticleApiVisualElement(v.resource, v.language)),
        introduction = article.introduction.map(i => api.ArticleApiIntroduction(i.introduction, i.language)),
        metaDescription = article.metaDescription.map(m => api.ArticleApiMetaDescription(m.content, m.language)),
        metaImageId = article.metaImageId,
        created = article.created,
        updated = article.updated,
        updatedBy = article.updatedBy,
        articleType = Some(article.articleType.toString)
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
      ArticleApiClient.allocateConceptId(None) match {
        case Failure(ex) => Failure(ex)
        case Success(id) =>
          Success(domain.Concept(
            Some(id),
            Seq(domain.ConceptTitle(concept.title, concept.language)),
            concept.content.map(content => Seq(domain.ConceptContent(content, concept.language))).getOrElse(Seq.empty),
            concept.copyright.map(toDomainCopyright),
            clock.now(),
            clock.now()
          ))
      }
    }

    def toDomainArticle(toMergeInto: domain.Article, article: api.UpdatedArticle, isImported: Boolean): domain.Article = {
      val lang = article.language
      val status = toMergeInto.status.filterNot(s => s == CREATED || s == PUBLISHED) + (if (!isImported) DRAFT else IMPORTED)
      toMergeInto.copy(
        status = status,
        revision = Option(article.revision),
        title = mergeLanguageFields(toMergeInto.title, article.title.toSeq.map(t => toDomainTitle(api.ArticleTitle(t, lang)))),
        content = mergeLanguageFields(toMergeInto.content, article.content.toSeq.map(c => toDomainContent(api.ArticleContent(c, lang)))),
        copyright = article.copyright.map(toDomainCopyright).orElse(toMergeInto.copyright),
        tags = mergeLanguageFields(toMergeInto.tags, toDomainTag(article.tags, lang)),
        requiredLibraries = article.requiredLibraries.map(toDomainRequiredLibraries),
        visualElement = mergeLanguageFields(toMergeInto.visualElement, article.visualElement.map(c => toDomainVisualElement(c, lang)).toSeq),
        introduction = mergeLanguageFields(toMergeInto.introduction, article.introduction.map(i => toDomainIntroduction(i, lang)).toSeq),
        metaDescription = mergeLanguageFields(toMergeInto.metaDescription, article.metaDescription.map(m => toDomainMetaDescription(m, lang)).toSeq),
        metaImageId = if (article.metaImageId.isDefined) article.metaImageId else toMergeInto.metaImageId,
        updated = clock.now(),
        updatedBy = authUser.userOrClientId(),
        articleType = article.articleType.map(ArticleType.valueOfOrError).getOrElse(toMergeInto.articleType)
      )
    }

    def toDomainArticle(id: Long, article: api.UpdatedArticle, isImported: Boolean): domain.Article = {
      val lang = article.language
      val status = if (isImported) Set(ArticleStatus.IMPORTED) else Set(ArticleStatus.CREATED)

      domain.Article(
        id = Some(id),
        revision = Some(1),
        status = status,
        title = article.title.map(t => domain.ArticleTitle(t, lang)).toSeq,
        content = article.content.map(c => domain.ArticleContent(c, lang)).toSeq,
        copyright = article.copyright.map(toDomainCopyright),
        tags = Seq(domain.ArticleTag(article.tags, lang)),
        requiredLibraries = article.requiredLibraries.map(toDomainRequiredLibraries),
        visualElement = article.visualElement.map(v => toDomainVisualElement(v, lang)).toSeq,
        introduction = article.introduction.map(i => toDomainIntroduction(i, lang)).toSeq,
        metaDescription = article.metaDescription.map(m => toDomainMetaDescription(m, lang)).toSeq,
        metaImageId = article.metaImageId,
        created = clock.now(),
        updated = clock.now(),
        authUser.userOrClientId(),
        articleType = article.articleType.map(ArticleType.valueOfOrError).getOrElse(ArticleType.Standard)
      )
    }

    def toDomainConcept(toMergeInto: domain.Concept, updateConcept: api.UpdatedConcept): domain.Concept = {
      val domainTitle = updateConcept.title.map(t => domain.ConceptTitle(t, updateConcept.language)).toSeq
      val domainContent = updateConcept.content.map(c => domain.ConceptContent(c, updateConcept.language)).toSeq

      toMergeInto.copy(
        title=mergeLanguageFields(toMergeInto.title, domainTitle),
        content=mergeLanguageFields(toMergeInto.content, domainContent),
        copyright=updateConcept.copyright.map(toDomainCopyright).orElse(toMergeInto.copyright),
        created=toMergeInto.created,
        updated=clock.now()
      )
    }

    private[service] def mergeLanguageFields[A <: LanguageField[_]](existing: Seq[A], updated: Seq[A]): Seq[A] = {
      val toKeep = existing.filterNot(item => updated.map(_.language).contains(item.language))
      (toKeep ++ updated).filterNot(_.isEmpty)
    }

  }
}
