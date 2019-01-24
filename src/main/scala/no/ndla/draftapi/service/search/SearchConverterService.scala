/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import com.sksamuel.elastic4s.http.search.SearchHit
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.model.api.{AgreementSearchResult, ArticleSearchResult, ConceptSearchResult}
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.search._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.service.ConverterService
import no.ndla.mapping.ISO639
import no.ndla.network.ApplicationUrl
import org.joda.time.DateTime
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.read
import org.jsoup.Jsoup

trait SearchConverterService {
  this: ConverterService =>
  val searchConverterService: SearchConverterService

  class SearchConverterService extends LazyLogging {
    implicit val formats: Formats = SearchableLanguageFormats.JSonFormats

    def asSearchableArticle(ai: Article): SearchableArticle = {

      val defaultTitle = ai.title
        .sortBy(title => {
          val languagePriority = Language.languageAnalyzers.map(la => la.lang).reverse
          languagePriority.indexOf(title.language)
        })
        .lastOption

      SearchableArticle(
        id = ai.id.get,
        title = SearchableLanguageValues(ai.title.map(title => LanguageValue(title.language, title.title))),
        visualElement =
          SearchableLanguageValues(ai.visualElement.map(visual => LanguageValue(visual.language, visual.resource))),
        introduction =
          SearchableLanguageValues(ai.introduction.map(intro => LanguageValue(intro.language, intro.introduction))),
        content = SearchableLanguageValues(
          ai.content.map(article => LanguageValue(article.language, Jsoup.parseBodyFragment(article.content).text()))),
        tags = SearchableLanguageList(ai.tags.map(tag => LanguageValue(tag.language, tag.tags))),
        lastUpdated = new DateTime(ai.updated),
        license = ai.copyright.flatMap(_.license),
        authors = ai.copyright
          .map(copy => copy.creators ++ copy.processors ++ copy.rightsholders)
          .map(a => a.map(_.name))
          .toSeq
          .flatten,
        articleType = ai.articleType.toString,
        notes = ai.notes.map(_.note),
        defaultTitle = defaultTitle.map(_.title)
      )
    }

    private def createUrlToArticle(id: Long): String = s"${ApplicationUrl.get}$id"

    def asSearchableConcept(c: Concept): SearchableConcept = {
      val defaultTitle = c.title
        .sortBy(title => {
          val languagePriority = Language.languageAnalyzers.map(la => la.lang).reverse
          languagePriority.indexOf(title.language)
        })
        .lastOption

      SearchableConcept(
        id = c.id.get,
        title = SearchableLanguageValues(c.title.map(title => LanguageValue(title.language, title.title))),
        content = SearchableLanguageValues(c.content.map(content => LanguageValue(content.language, content.content))),
        defaultTitle = defaultTitle.map(_.title)
      )
    }

    def hitAsArticleSummary(hitString: String, language: String): api.ArticleSummary = {
      val searchableArticle = read[SearchableArticle](hitString)

      val titles = searchableArticle.title.languageValues.map(lv => domain.ArticleTitle(lv.value, lv.language))
      val introductions =
        searchableArticle.introduction.languageValues.map(lv => domain.ArticleIntroduction(lv.value, lv.language))
      val visualElements =
        searchableArticle.visualElement.languageValues.map(lv => domain.VisualElement(lv.value, lv.language))
      val notes = searchableArticle.notes

      val supportedLanguages = getSupportedLanguages(Seq(titles, visualElements, introductions))

      val title = findByLanguageOrBestEffort(titles, language)
        .map(converterService.toApiArticleTitle)
        .getOrElse(api.ArticleTitle("", UnknownLanguage))
      val visualElement = findByLanguageOrBestEffort(visualElements, language).map(converterService.toApiVisualElement)
      val introduction =
        findByLanguageOrBestEffort(introductions, language).map(converterService.toApiArticleIntroduction)

      api.ArticleSummary(
        searchableArticle.id,
        title,
        visualElement,
        introduction,
        ApplicationUrl.get + searchableArticle.id,
        searchableArticle.license.getOrElse(""),
        searchableArticle.articleType,
        supportedLanguages,
        notes
      )
    }

    def hitAsConceptSummary(hitString: String, language: String): api.ConceptSummary = {

      val searchableConcept = read[SearchableConcept](hitString)
      val titles = searchableConcept.title.languageValues.map(lv => domain.ConceptTitle(lv.value, lv.language))
      val contents = searchableConcept.content.languageValues.map(lv => domain.ConceptContent(lv.value, lv.language))

      val supportedLanguages = getSupportedLanguages(Seq(titles, contents))

      val title = Language
        .findByLanguageOrBestEffort(titles, language)
        .map(converterService.toApiConceptTitle)
        .getOrElse(api.ConceptTitle("", Language.UnknownLanguage))
      val concept = Language
        .findByLanguageOrBestEffort(contents, language)
        .map(converterService.toApiConceptContent)
        .getOrElse(api.ConceptContent("", Language.UnknownLanguage))

      api.ConceptSummary(
        searchableConcept.id,
        title,
        concept,
        supportedLanguages
      )
    }

    def asSearchableAgreement(domainModel: Agreement): SearchableAgreement = {
      SearchableAgreement(
        id = domainModel.id.get,
        title = domainModel.title,
        content = domainModel.content,
        license = domainModel.copyright.license.get
      )
    }

    def hitAsAgreementSummary(hitString: String): api.AgreementSummary = {
      val hit = parse(hitString)
      val id = (hit \ "id").extract[Long]
      val title = (hit \ "title").extract[String]
      val license = (hit \ "license").extract[String]

      api.AgreementSummary(id, title, license)
    }

    /**
      * Attempts to extract language that hit from highlights in elasticsearch response.
      *
      * @param result Elasticsearch hit.
      * @return Language if found.
      */
    def getLanguageFromHit(result: SearchHit): Option[String] = {
      def keyToLanguage(keys: Iterable[String]): Option[String] = {
        val keyLanguages = keys.toList.flatMap(key =>
          key.split('.').toList match {
            case _ :: language :: _ => Some(language)
            case _                  => None
        })

        keyLanguages
          .sortBy(lang => {
            ISO639.languagePriority.reverse.indexOf(lang)
          })
          .lastOption
      }

      val highlightKeys: Option[Map[String, _]] = Option(result.highlight)
      val matchLanguage = keyToLanguage(highlightKeys.getOrElse(Map()).keys)

      matchLanguage match {
        case Some(lang) =>
          Some(lang)
        case _ =>
          keyToLanguage(result.sourceAsMap.keys)
      }
    }

    def asApiSearchResult(searchResult: SearchResult[api.ArticleSummary]): ArticleSearchResult =
      api.ArticleSearchResult(
        searchResult.totalCount,
        searchResult.page,
        searchResult.pageSize,
        searchResult.results
      )

    def asApiAgreementSearchResult(searchResult: SearchResult[api.AgreementSummary]): AgreementSearchResult =
      api.AgreementSearchResult(searchResult.totalCount,
                                searchResult.page,
                                searchResult.pageSize,
                                searchResult.language,
                                searchResult.results)

    def asApiConceptSearchResult(searchResult: SearchResult[api.ConceptSummary]): ConceptSearchResult =
      api.ConceptSearchResult(searchResult.totalCount,
                              searchResult.page,
                              searchResult.pageSize,
                              searchResult.language,
                              searchResult.results)

  }
}
