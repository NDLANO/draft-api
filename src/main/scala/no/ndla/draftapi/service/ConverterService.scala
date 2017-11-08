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
import no.ndla.draftapi.model.domain.Language
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.validation.HtmlTools
import no.ndla.mapping.License.getLicense
import no.ndla.network.ApplicationUrl
import Language._
import no.ndla.draftapi.model.api.NewAgreement

import scala.collection.JavaConverters._

trait ConverterService {
  this: Clock with DraftRepository with User =>
  val converterService: ConverterService

  class ConverterService extends LazyLogging {
    def getAgreementHits(response: JestSearchResult): Seq[api.AgreementSummary] = {
      var resultList = Seq[api.AgreementSummary]() //TODO: Look into why this is var
      response.getTotal match {
        case count: Integer if count > 0 =>
          val resultArray = response.getJsonObject.get("hits").asInstanceOf[JsonObject].get("hits").getAsJsonArray
          val iterator = resultArray.iterator()
          while (iterator.hasNext) {
            resultList = resultList :+ hitAsAgreementSummary(iterator.next().asInstanceOf[JsonObject].get("_source").asInstanceOf[JsonObject])
          }
          resultList
        case _ => Seq()
      }
    }


    def getHitsV2(response: JestSearchResult, language: String): Seq[api.ArticleSummary] = {
      var resultList = Seq[api.ArticleSummary]()
      response.getTotal match {
        case count: Integer if count > 0 => {
          val resultArray = response.getJsonObject.get("hits").asInstanceOf[JsonObject].get("hits").getAsJsonArray
          val iterator = resultArray.iterator()
          while (iterator.hasNext) {
            resultList = resultList :+ hitAsArticleSummaryV2(iterator.next().asInstanceOf[JsonObject].get("_source").asInstanceOf[JsonObject], language)
          }
          resultList
        }
        case _ => Seq()
      }
    }

    def hitAsAgreementSummary(hit: JsonObject): api.AgreementSummary = ???

    def hitAsArticleSummaryV2(hit: JsonObject, language: String): api.ArticleSummary = {
      val titles = getEntrySetSeq(hit, "title").map(entr => domain.ArticleTitle(entr.getValue.getAsString, entr.getKey))
      val introductions = getEntrySetSeq(hit, "introduction").map(entr => domain.ArticleIntroduction (entr.getValue.getAsString, entr.getKey))
      val visualElements = getEntrySetSeq(hit, "visualElement").map(entr => domain.VisualElement(entr.getValue.getAsString, entr.getKey))

      val supportedLanguages =  getSupportedLanguages(Seq(titles, visualElements, introductions))

      val title = findByLanguageOrBestEffort(titles, language).map(converterService.toApiArticleTitle).getOrElse(api.ArticleTitle("", DefaultLanguage))
      val visualElement = findByLanguageOrBestEffort(visualElements, language).map(converterService.toApiVisualElement)
      val introduction = findByLanguageOrBestEffort(introductions, language).map(converterService.toApiArticleIntroduction)

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
      val domainTitle = Seq(domain.ArticleTitle(newArticle.title, newArticle.language))
      val domainContent = Seq(domain.ArticleContent(
        removeUnknownEmbedTagAttributes(newArticle.content),
        newArticle.language)
      )

      domain.Article(
        id=None,
        revision=None,
        title=domainTitle,
        content=domainContent,
        copyright=toDomainCopyright(newArticle.copyright),
        tags=toDomainTagV2(newArticle.tags, newArticle.language),
        requiredLibraries=newArticle.requiredLibraries.getOrElse(Seq()).map(toDomainRequiredLibraries),
        visualElement=toDomainVisualElementV2(newArticle.visualElement, newArticle.language),
        introduction=toDomainIntroductionV2(newArticle.introduction, newArticle.language),
        metaDescription=toDomainMetaDescriptionV2(newArticle.metaDescription, newArticle.language),
        metaImageId=newArticle.metaImageId,
        created=clock.now(),
        updated=clock.now(),
        updatedBy=authUser.id(),
        newArticle.articleType
      )
    }

    def toDomainAgreement(newAgreement: NewAgreement): domain.Agreement = {
      domain.Agreement(
        id = None,
        content = newAgreement.content,
        copyright = toDomainCopyright(newAgreement.copyright),
        created = clock.now(),
        updated = clock.now(),
        updatedBy = authUser.id()
      )
    }

    def toDomainTitle(articleTitle: api.ArticleTitle): domain.ArticleTitle = {
      domain.ArticleTitle(articleTitle.title, articleTitle.language)
    }

    def toDomainContent(articleContent: api.ArticleContentV2): domain.ArticleContent = {
      domain.ArticleContent(removeUnknownEmbedTagAttributes(articleContent.content), articleContent.language)
    }

    def toDomainTag(tag: api.ArticleTag): domain.ArticleTag = {
      domain.ArticleTag(tag.tags, tag.language)
    }

    def toDomainTagV2(tag: Seq[String], language: String): Seq[domain.ArticleTag] = {
      if (tag.isEmpty) {
        Seq.empty[domain.ArticleTag]
      } else {
        Seq(domain.ArticleTag(tag, language))
      }
    }

    def toDomainVisualElement(visual: api.VisualElement): domain.VisualElement = {
      domain.VisualElement(removeUnknownEmbedTagAttributes(visual.visualElement), visual.language)
    }

    def toDomainVisualElementV2(visual: Option[String], language: String): Seq[domain.VisualElement] = {
      if (visual.isEmpty) {
        Seq.empty[domain.VisualElement]
      } else {
        Seq(domain.VisualElement(removeUnknownEmbedTagAttributes(visual.getOrElse("")), language))
      }
    }

    def toDomainIntroduction(intro: api.ArticleIntroduction): domain.ArticleIntroduction = {
      domain.ArticleIntroduction(intro.introduction, intro.language)
    }

    def toDomainIntroductionV2(intro: Option[String], language: String): Seq[domain.ArticleIntroduction] = {
      if (intro.isEmpty) {
        Seq.empty[domain.ArticleIntroduction]
      } else {
        Seq(domain.ArticleIntroduction(intro.getOrElse(""), language))
      }
    }

    def toDomainMetaDescription(meta: api.ArticleMetaDescription): domain.ArticleMetaDescription = {
      domain.ArticleMetaDescription(meta.metaDescription, meta.language)
    }

    def toDomainMetaDescriptionV2(meta: Option[String], language: String): Seq[domain.ArticleMetaDescription]= {
      if (meta.isEmpty) {
        Seq.empty[domain.ArticleMetaDescription]
      } else {
        Seq(domain.ArticleMetaDescription(meta.getOrElse(""), language))
      }
    }

   def toDomainCopyright(copyright: api.Copyright): domain.Copyright = {
      domain.Copyright(
        copyright.license.license,
        copyright.origin,
        copyright.creators.map(toDomainAuthor),
        copyright.processors.map(toDomainAuthor),
        copyright.rightsholders.map(toDomainAuthor),
        copyright.agreement,
        copyright.validFrom,
        copyright.validTo
      )
    }

    def toDomainAuthor(author: api.Author): domain.Author = {
      domain.Author(author.`type`, author.name)
    }

    def toDomainRequiredLibraries(requiredLibs: api.RequiredLibrary): domain.RequiredLibrary = {
      domain.RequiredLibrary(requiredLibs.mediaType, requiredLibs.name, requiredLibs.url)
    }

    private def getLinkToOldNdla(id: Long): Option[String] = {
      draftRepository.getExternalIdFromId(id).map(createLinkToOldNdla)
    }

    private def removeUnknownEmbedTagAttributes(html: String): String = {
      val document = HtmlTools.stringToJsoupDocument(html)
      document.select("embed").asScala.map(el => {
        domain.ResourceType.valueOf(el.attr(domain.Attributes.DataResource.toString))
          .map(domain.EmbedTag.attributesForResourceType)
          .map(knownAttributes => HtmlTools.removeIllegalAttributes(el, knownAttributes.all.map(_.toString)))
      })

      HtmlTools.jsoupDocumentToString(document)
    }

    def toApiArticleV2(article: domain.Article, language: String): Option[api.Article] = {
      val supportedLanguages = getSupportedLanguages(
        Seq(article.title, article.visualElement, article.introduction, article.metaDescription, article.tags, article.content)
      )

      if (supportedLanguages.isEmpty || (!supportedLanguages.contains(language) && language != AllLanguages)) return None
      val searchLanguage = getSearchLanguage(language, supportedLanguages)

      val meta = findByLanguageOrBestEffort(article.metaDescription, language).map(toApiArticleMetaDescription).getOrElse(api.ArticleMetaDescription("", DefaultLanguage))
      val tags = findByLanguageOrBestEffort(article.tags, language).map(toApiArticleTag).getOrElse(api.ArticleTag(Seq(), DefaultLanguage))
      val title = findByLanguageOrBestEffort(article.title, language).map(toApiArticleTitle).getOrElse(api.ArticleTitle("", DefaultLanguage))
      val introduction = findByLanguageOrBestEffort(article.introduction, language).map(toApiArticleIntroduction)
      val visualElement = findByLanguageOrBestEffort(article.visualElement, language).map(toApiVisualElement)
      val articleContent = findByLanguageOrBestEffort(article.content, language).map(toApiArticleContentV2).getOrElse(api.ArticleContentV2("", DefaultLanguage))


      Some(api.Article(
        article.id.get,
        article.id.flatMap(getLinkToOldNdla),
        article.revision.get,
        title,
        articleContent,
        toApiCopyright(article.copyright),
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
      ))
    }

    def toApiAgreement(agreement: domain.Agreement): api.Agreement = ???

    def toApiArticleTitle(title: domain.ArticleTitle): api.ArticleTitle = {
      api.ArticleTitle(title.title, title.language)
    }

    def toApiArticleContentV2(content: domain.ArticleContent): api.ArticleContentV2 = {
      api.ArticleContentV2(
        content.content,
        content.language
      )
    }

    def toApiCopyright(copyright: domain.Copyright): api.Copyright = {
      api.Copyright(
        toApiLicense(copyright.license),
        copyright.origin,
        copyright.creators.map(toApiAuthor),
        copyright.processors.map(toApiAuthor),
        copyright.rightsholders.map(toApiAuthor),
        copyright.agreement,
        copyright.validFrom,
        copyright.validTo
      )
    }

    def toApiLicense(shortLicense: String): api.License = {
      getLicense(shortLicense) match {
        case Some(l) => api.License(l.license, Option(l.description), l.url)
        case None => api.License("unknown", None, None)
      }
    }

    def toApiAuthor(author: domain.Author): api.Author = {
      api.Author(author.`type`, author.name)
    }

    def toApiArticleTag(tag: domain.ArticleTag): api.ArticleTag = {
      api.ArticleTag(tag.tags, tag.language)
    }

    def toApiRequiredLibrary(required: domain.RequiredLibrary): api.RequiredLibrary = {
      api.RequiredLibrary(required.mediaType, required.name, required.url)
    }

    def toApiVisualElement(visual: domain.VisualElement): api.VisualElement = {
      api.VisualElement(visual.resource, visual.language)
    }

    def toApiArticleIntroduction(intro: domain.ArticleIntroduction): api.ArticleIntroduction = {
      api.ArticleIntroduction(intro.introduction, intro.language)
    }

    def toApiArticleMetaDescription(metaDescription: domain.ArticleMetaDescription): api.ArticleMetaDescription= {
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

    def toApiConceptContent(title: domain.ConceptContent): api.ConceptContent= api.ConceptContent(title.content, title.language)

  }
}
