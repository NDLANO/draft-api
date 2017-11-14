/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service

import java.util.Map.Entry

import com.google.gson.{JsonElement, JsonObject}
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.auth.User
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.ArticleApiCopyright
import no.ndla.draftapi.model.domain.{ArticleStatus, ArticleType}
import no.ndla.draftapi.model.domain.ArticleStatus._
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.mapping.License.getLicense
import no.ndla.network.ApplicationUrl
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

    def toDomainArticle(newArticle: api.NewArticle): Try[domain.Article] = {
      ArticleApiClient.allocateArticleId match {
        case Failure(ex) => Failure(ex)
        case Success(id) =>
          val domainTitles = Seq(domain.ArticleTitle(newArticle.title, newArticle.language))
          val domainContent = newArticle.content.map(content => domain.ArticleContent(removeUnknownEmbedTagAttributes(content), newArticle.language)).toSeq

          Success(domain.Article(
            id = Some(id),
            revision = None,
            ArticleStatus.ValueSet(CREATED),
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
            updatedBy = authUser.id(),
            articleType = newArticle.articleType.flatMap(ArticleType.valueOf)
          ))
      }
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
      domain.Copyright(copyright.license.map(_.license), copyright.origin, copyright.authors.map(toDomainAuthor))
    }

    def toDomainAuthor(author: api.Author): domain.Author = domain.Author(author.`type`, author.name)

    def toDomainRequiredLibraries(requiredLibs: api.RequiredLibrary): domain.RequiredLibrary = {
      domain.RequiredLibrary(requiredLibs.mediaType, requiredLibs.name, requiredLibs.url)
    }

    private def getLinkToOldNdla(id: Long): Option[String] = draftRepository.getExternalIdFromId(id).map(createLinkToOldNdla)

    private def removeUnknownEmbedTagAttributes(html: String): String = {
      val document = HtmlRules.stringToJsoupDocument(html)
      document.select("embed").asScala.map(el => {
        ResourceType.valueOf(el.attr(Attributes.DataResource.toString))
          .map(EmbedTagRules.attributesForResourceType)
          .map(knownAttributes => HtmlRules.removeIllegalAttributes(el, knownAttributes.all.map(_.toString)))
      })

      HtmlRules.jsoupDocumentToString(document)
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
        article.articleType.map(_.toString),
        supportedLanguages
      )
    }

    def toDomainStatus(status: api.ArticleStatus): Try[Set[ArticleStatus.Value]] = {
      val (validStatuses, invalidStatuses) = status.status.map(ArticleStatus.valueOfOrError).partition(_.isSuccess)
      if (invalidStatuses.nonEmpty) {
        val errors = invalidStatuses.flatMap {
          case Failure(ex: ValidationException) => ex.errors
          case Failure(ex) => Set(ValidationMessage("status", ex.getMessage))
        }
        Failure(new ValidationException(errors=errors.toSeq))
      } else
        Success(validStatuses.map(_.get))
    }

    def toApiArticleTitle(title: domain.ArticleTitle): api.ArticleTitle = api.ArticleTitle(title.title, title.language)

    def toApiArticleContent(content: domain.ArticleContent): api.ArticleContent = api.ArticleContent(content.content, content.language)

    def toApiCopyright(copyright: domain.Copyright): api.Copyright = {
      api.Copyright(copyright.license.map(toApiLicense), copyright.origin, copyright.authors.map(toApiAuthor))
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
        title,
        content,
        concept.copyright.map(toApiCopyright),
        concept.created,
        concept.updated,
        concept.supportedLanguages
      )
    }

    def toApiConceptTitle(title: domain.ConceptTitle): api.ConceptTitle = api.ConceptTitle(title.title, title.language)

    def toApiConceptContent(title: domain.ConceptContent): api.ConceptContent = api.ConceptContent(title.content, title.language)

    def toArticleApiArticle(article: domain.Article): api.ArticleApiArticle = {
      api.ArticleApiArticle(
        revision = article.revision,
        title = article.title.map(t => api.ArticleApiTitle(t.title, t.language)),
        content = article.content.map(c => api.ArticleApiContent(c.content, c.language)),
        copyright = article.copyright.map(c => api.ArticleApiCopyright(c.license, c.origin, c.authors.map(a => api.ArticleApiAuthor(a.`type`, a.name)))),
        tags = article.tags.map(t => api.ArticleApiTag(t.tags, t.language)),
        requiredLibraries = article.requiredLibraries.map(r => api.ArticleApiRequiredLibrary(r.mediaType, r.name, r.url)),
        visualElement = article.visualElement.map(v => api.ArticleApiVisualElement(v.resource, v.language)),
        introduction = article.introduction.map(i => api.ArticleApiIntroduction(i.introduction, i.language)),
        metaDescription = article.metaDescription.map(m => api.ArticleApiMetaDescription(m.content, m.language)),
        metaImageId = article.metaImageId,
        created = article.created,
        updated = article.updated,
        updatedBy = article.updatedBy,
        articleType = article.articleType.map(_.toString)
      )
    }

  }
}
