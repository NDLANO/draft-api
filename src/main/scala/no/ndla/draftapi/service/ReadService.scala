/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.DraftApiProperties.{externalApiUrls, resourceHtmlEmbedTag}
import no.ndla.draftapi.caching.MemoizeAutoRenew
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.repository.{ConceptRepository, DraftRepository}
import no.ndla.validation.{Attributes, HtmlRules}
import org.jsoup.nodes.Element

import scala.math.max
import scala.collection.JavaConverters._

trait ReadService {
  this: DraftRepository with ConceptRepository with ConverterService =>
  val readService: ReadService

  class ReadService {
    def getInternalIdByExternalId(externalId: Long): Option[api.ArticleId] =
      draftRepository.getIdFromExternalId(externalId.toString).map(api.ArticleId)

    def withId(id: Long, language: String): Option[api.Article] = {
      draftRepository.withId(id)
        .map(addUrlsOnEmbedResources)
        .map(article => converterService.toApiArticle(article, language))
    }

    private[service] def addUrlsOnEmbedResources(article: domain.Article): domain.Article = {
      val articleWithUrls = article.content.map(content => content.copy(content = addUrlOnResource(content.content)))
      val visualElementWithUrls = article.visualElement.map(visual => visual.copy(resource = addUrlOnResource(visual.resource)))

      article.copy(content = articleWithUrls, visualElement = visualElementWithUrls)
    }

    def getNMostUsedTags(n: Int, language: String): Option[api.ArticleTag] = {
      val tagUsageMap = getTagUsageMap()
      val searchLanguage = getSearchLanguage(language, supportedLanguages)

      tagUsageMap.get(searchLanguage)
        .map(tags => api.ArticleTag(tags.getNMostFrequent(n), searchLanguage))
    }

    def getArticlesByPage(pageNo: Int, pageSize: Int, lang: String): api.ArticleDump = {
      val (safePageNo, safePageSize) = (max(pageNo, 1), max(pageSize, 0))
      val results = draftRepository.getArticlesByPage(safePageSize, (safePageNo - 1) * safePageSize).map(article => converterService.toApiArticle(article, lang))
      api.ArticleDump(draftRepository.articleCount, pageNo, pageSize, lang, results)
    }

    val getTagUsageMap = MemoizeAutoRenew(() => {
      draftRepository.allTags.map(languageTags => languageTags.language -> new MostFrequentOccurencesList(languageTags.tags)).toMap
    })

    private[service] def addUrlOnResource(content: String): String = {
      val doc = HtmlRules.stringToJsoupDocument(content)

      val embedTags = doc.select(s"$resourceHtmlEmbedTag").asScala.toList
      embedTags.foreach(addUrlOnEmbedTag)
      HtmlRules.jsoupDocumentToString(doc)
    }

    private def addUrlOnEmbedTag(embedTag: Element) = {
      val resourceIdAttrName = Attributes.DataResource_Id.toString
      embedTag.hasAttr(resourceIdAttrName) match {
        case false =>
        case true => {
          val (resourceType, id) = (embedTag.attr(s"${Attributes.DataResource}"), embedTag.attr(resourceIdAttrName))
          embedTag.attr(s"${Attributes.DataUrl}", s"${externalApiUrls(resourceType)}/$id")
        }
      }
    }

    class MostFrequentOccurencesList(list: Seq[String]) {
      // Create a map where the key is a list entry, and the value is the number of occurences of this entry in the list
      private[this] val listToNumOccurencesMap: Map[String, Int] = list.groupBy(identity).mapValues(_.size)
      // Create an inverse of the map 'listToNumOccurencesMap': the key is number of occurences, and the value is a list of all entries that occured that many times
      private[this] val numOccurencesToListMap: Map[Int, Set[String]] = listToNumOccurencesMap.groupBy(x => x._2).mapValues(_.keySet)
      // Build a list sorted by the most frequent words to the least frequent words
      private[this] val mostFrequentOccorencesDec = numOccurencesToListMap.keys.toSeq.sorted
        .foldRight(Seq[String]())((current, result) => result ++ numOccurencesToListMap(current))

      def getNMostFrequent(n: Int): Seq[String] = mostFrequentOccorencesDec.slice(0, n)
    }

    def conceptWithId(id: Long, language: String): Option[api.Concept] =
      conceptRepository.withId(id).map(concept => converterService.toApiConcept(concept, language))

  }

}
