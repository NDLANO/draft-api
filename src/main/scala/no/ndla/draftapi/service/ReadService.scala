/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import io.lemonlabs.uri.{Path, Url}
import no.ndla.draftapi.DraftApiProperties.{Domain, externalApiUrls, resourceHtmlEmbedTag}
import no.ndla.draftapi.caching.MemoizeAutoRenew
import no.ndla.draftapi.model.api.NotFoundException
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.api.NotFoundException
import no.ndla.draftapi.model.domain.{ArticleIds, ImportId}
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.repository.{AgreementRepository, DraftRepository}
import no.ndla.validation._
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._
import scala.math.max
import scala.util.{Failure, Success, Try}

trait ReadService {
  this: DraftRepository with AgreementRepository with ConverterService =>
  val readService: ReadService

  class ReadService {

    def getInternalArticleIdByExternalId(externalId: Long): Option[api.ContentId] =
      draftRepository.getIdFromExternalId(externalId.toString).map(api.ContentId)

    def withId(id: Long, language: String, fallback: Boolean = false): Try[api.Article] = {
      draftRepository.withId(id).map(addUrlsOnEmbedResources) match {
        case None          => Failure(NotFoundException(s"The article with id $id was not found"))
        case Some(article) => converterService.toApiArticle(article, language, fallback)
      }
    }

    def getArticles(id: Long, language: String, fallback: Boolean): Seq[api.Article] = {
      draftRepository
        .articlesWithId(id)
        .map(addUrlsOnEmbedResources)
        .map(article => converterService.toApiArticle(article, language, fallback))
        .collect { case Success(article) => article }
        .sortBy(_.revision)
        .reverse
    }

    private[service] def addUrlsOnEmbedResources(article: domain.Article): domain.Article = {
      val articleWithUrls = article.content.map(content => content.copy(content = addUrlOnResource(content.content)))
      val visualElementWithUrls =
        article.visualElement.map(visual => visual.copy(resource = addUrlOnResource(visual.resource)))

      article.copy(content = articleWithUrls, visualElement = visualElementWithUrls)
    }

    def getNMostUsedTags(n: Int, language: String): Option[api.ArticleTag] = {
      val tagUsageMap = getTagUsageMap()
      val searchLanguage = getSearchLanguage(language, supportedLanguages)

      tagUsageMap
        .flatMap(_.get(searchLanguage))
        .map(tags => api.ArticleTag(tags.getNMostFrequent(n), searchLanguage))
    }

    def getArticlesByPage(pageNo: Int, pageSize: Int, lang: String, fallback: Boolean = false): api.ArticleDump = {
      val (safePageNo, safePageSize) = (max(pageNo, 1), max(pageSize, 0))
      val results = draftRepository
        .getArticlesByPage(safePageSize, (safePageNo - 1) * safePageSize)
        .flatMap(article => converterService.toApiArticle(article, lang, fallback).toOption)
      api.ArticleDump(draftRepository.articleCount, pageNo, pageSize, lang, results)
    }

    def getArticleDomainDump(pageNo: Int, pageSize: Int): api.ArticleDomainDump = {
      val (safePageNo, safePageSize) = (max(pageNo, 1), max(pageSize, 0))
      val results = draftRepository.getArticlesByPage(safePageSize, (safePageNo - 1) * safePageSize)

      api.ArticleDomainDump(draftRepository.articleCount, pageNo, pageSize, results)
    }

    def getAllCompetences(input: String, pageSize: Int, offset: Int): api.CompetencesSearchResult = {
      val (competences, competenceCount) = draftRepository.getCompetences(input, pageSize, (offset - 1) * pageSize)
      converterService.toApiArticleCompetences(competences, competenceCount, pageSize, offset)
    }

    def getAllTags(input: String, pageSize: Int, offset: Int, language: String): api.TagsSearchResult = {
      val (tags, tagsCount) = draftRepository.getTags(input, pageSize, (offset - 1) * pageSize, language)
      converterService.toApiArticleTags(tags, tagsCount, pageSize, offset, language)
    }

    val getTagUsageMap = MemoizeAutoRenew(() => {
      draftRepository.allTags
        .map(languageTags => languageTags.language -> new MostFrequentOccurencesList(languageTags.tags))
        .toMap
    })

    private[service] def addUrlOnResource(content: String): String = {
      val doc = HtmlTagRules.stringToJsoupDocument(content)

      val embedTags = doc.select(s"$resourceHtmlEmbedTag").asScala.toList
      embedTags.foreach(addUrlOnEmbedTag)
      HtmlTagRules.jsoupDocumentToString(doc)
    }

    private def addUrlOnEmbedTag(embedTag: Element): Unit = {
      val typeAndPathOption = embedTag.attr(TagAttributes.DataResource.toString) match {
        case resourceType
            if resourceType == ResourceType.File.toString && embedTag.hasAttr(TagAttributes.DataPath.toString) =>
          val path = embedTag.attr(TagAttributes.DataPath.toString)
          Some((resourceType, path))

        case resourceType if embedTag.hasAttr(TagAttributes.DataResource_Id.toString) =>
          val id = embedTag.attr(TagAttributes.DataResource_Id.toString)
          Some((resourceType, id))
        case _ =>
          None
      }

      typeAndPathOption match {
        case Some((resourceType, path)) =>
          val baseUrl = Url.parse(externalApiUrls(resourceType))
          val pathParts = Path.parse(path).parts

          embedTag.attr(
            s"${TagAttributes.DataUrl}",
            baseUrl.addPathParts(pathParts).toString
          )
        case _ =>
      }
    }

    class MostFrequentOccurencesList(list: Seq[String]) {
      // Create a map where the key is a list entry, and the value is the number of occurrences of this entry in the list
      private[this] val listToNumOccurrencesMap: Map[String, Int] = list.groupBy(identity).mapValues(_.size)
      // Create an inverse of the map 'listToNumOccurrencesMap': the key is number of occurrences, and the value is a list of all entries that occurred that many times
      private[this] val numOccurrencesToListMap: Map[Int, Set[String]] =
        listToNumOccurrencesMap.groupBy(x => x._2).mapValues(_.keySet)
      // Build a list sorted by the most frequent words to the least frequent words
      private[this] val mostFrequentOccorencesDec = numOccurrencesToListMap.keys.toSeq.sorted
        .foldRight(Seq[String]())((current, result) => result ++ numOccurrencesToListMap(current))

      def getNMostFrequent(n: Int): Seq[String] = mostFrequentOccorencesDec.slice(0, n)
    }

    def agreementWithId(id: Long): Option[api.Agreement] =
      agreementRepository.withId(id).map(agreement => converterService.toApiAgreement(agreement))

    def importIdOfArticle(externalId: String): Option[ImportId] = {
      draftRepository.importIdOfArticle(externalId)
    }
  }
}
