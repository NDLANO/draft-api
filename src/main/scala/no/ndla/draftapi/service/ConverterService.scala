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
import io.searchbox.core.{SearchResult => JestSearchResult}
import no.ndla.draftapi.auth.User
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.domain.{ArticleStatus, Language}
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.mapping.License.getLicense
import no.ndla.network.ApplicationUrl
import Language._
import no.ndla.validation.{Attributes, EmbedTagRules, HtmlRules, ResourceType}

import scala.collection.JavaConverters._

trait ConverterService {
  this: Clock with DraftRepository with User =>
  val converterService: ConverterService

  class ConverterService extends LazyLogging {

    def getHits(response: JestSearchResult, language: String): Seq[api.ArticleSummary] = {
      var resultList = Seq[api.ArticleSummary]()
      response.getTotal match {
        case count: Integer if count > 0 => {
          val resultArray = response.getJsonObject.get("hits").asInstanceOf[JsonObject].get("hits").getAsJsonArray
          val iterator = resultArray.iterator()
          while (iterator.hasNext) {
            resultList = resultList :+ hitAsArticleSummary(iterator.next().asInstanceOf[JsonObject].get("_source").asInstanceOf[JsonObject], language)
          }
          resultList
        }
        case _ => Seq()
      }
    }

    def hitAsArticleSummary(hit: JsonObject, language: String): api.ArticleSummary = {
      val titles = getEntrySetSeq(hit, "title").map(entr => domain.ArticleTitle(entr.getValue.getAsString, entr.getKey))
      val introductions = getEntrySetSeq(hit, "introduction").map(entr => domain.ArticleIntroduction(entr.getValue.getAsString, entr.getKey))
      val visualElements = getEntrySetSeq(hit, "visualElement").map(entr => domain.VisualElement(entr.getValue.getAsString, entr.getKey))

      val supportedLanguages = getSupportedLanguages(Seq(titles, visualElements, introductions))

      val title = findByLanguageOrBestEffort(titles, language).map(toApiArticleTitle).getOrElse(api.ArticleTitle("", DefaultLanguage))
      val visualElement = findByLanguageOrBestEffort(visualElements, language).map(toApiVisualElement)
      val introduction = findByLanguageOrBestEffort(introductions, language).map(toApiArticleIntroduction)

      api.ArticleSummary(
        hit.get("id").getAsLong,
        title,
        visualElement,
        introduction,
        ApplicationUrl.get + hit.get("id").getAsString,
        hit.get("license").getAsString,
        hit.get("articleType").getAsString,
        supportedLanguages
      )
    }

    def getEntrySetSeq(hit: JsonObject, fieldPath: String): Seq[Entry[String, JsonElement]] = {
      hit.get(fieldPath).getAsJsonObject.entrySet.asScala.to[Seq]
    }

    def getValueByFieldAndLanguage(hit: JsonObject, fieldPath: String, searchLanguage: String): String = {
      hit.get(fieldPath).getAsJsonObject.entrySet.asScala.to[Seq].find(entr => entr.getKey == searchLanguage) match {
        case Some(element) => element.getValue.getAsString
        case None => ""
      }
    }

    def toDomainArticle(newArticle: api.NewArticle): domain.Article = {
      val domainTitles = Seq(domain.ArticleTitle(newArticle.title, newArticle.language))
      val domainContent = newArticle.content.map(content => domain.ArticleContent(removeUnknownEmbedTagAttributes(content), newArticle.language)).toSeq

      domain.Article(
        id = None,
        revision = None,
        ArticleStatus.DRAFT,
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
        articleType = newArticle.articleType
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
        article.status.toString,
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
        article.articleType,
        supportedLanguages
      )
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

  }
}
