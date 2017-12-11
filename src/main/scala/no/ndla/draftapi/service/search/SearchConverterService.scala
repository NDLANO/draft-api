/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import com.google.gson.{JsonElement, JsonObject}
import java.util.Map.Entry
import io.searchbox.core.{SearchResult => JestSearchResult}
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.search._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.network.ApplicationUrl
import org.jsoup.Jsoup
import scala.collection.JavaConverters._
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.service.ConverterService

trait SearchConverterService {
  this: ConverterService =>
  val searchConverterService: SearchConverterService

  class SearchConverterService extends LazyLogging {
    def asSearchableArticle(ai: Article): SearchableArticle = {
      SearchableArticle(
        id = ai.id.get,
        title = SearchableLanguageValues(ai.title.map(title => LanguageValue(title.language, title.title))),
        visualElement = SearchableLanguageValues(ai.visualElement.map(visual => LanguageValue(visual.language, visual.resource))),
        introduction = SearchableLanguageValues(ai.introduction.map(intro => LanguageValue(intro.language, intro.introduction))),
        content = SearchableLanguageValues(ai.content.map(article => LanguageValue(article.language, Jsoup.parseBodyFragment(article.content).text()))),
        tags = SearchableLanguageList(ai.tags.map(tag => LanguageValue(tag.language, tag.tags))),
        lastUpdated = ai.updated,
        license = ai.copyright.flatMap(_.license),
        authors = ai.copyright.map(copy => copy.creators ++ copy.processors ++ copy.rightsholders).map(a => a.map(_.name)).toSeq.flatten,
        articleType = ai.articleType.toString
      )
    }

    def asArticleSummary(searchableArticle: SearchableArticle): ArticleSummary = {
      ArticleSummary(
        id = searchableArticle.id,
        title = searchableArticle.title.languageValues.map(lv => ArticleTitle(lv.value, lv.lang)),
        visualElement = searchableArticle.visualElement.languageValues.map(lv => VisualElement(lv.value, lv.lang)),
        introduction = searchableArticle.introduction.languageValues.map(lv => ArticleIntroduction(lv.value, lv.lang)),
        url = createUrlToArticle(searchableArticle.id),
        license = searchableArticle.license)
    }

    private def createUrlToArticle(id: Long): String = s"${ApplicationUrl.get}$id"

    def asSearchableConcept(c: Concept): SearchableConcept = {
      SearchableConcept(
        c.id.get,
        SearchableLanguageValues(c.title.map(title => LanguageValue(title.language, title.title))),
        SearchableLanguageValues(c.content.map(content => LanguageValue(content.language, content.content)))
      )
    }

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

    def asSearchableAgreement(domainModel: Agreement): SearchableAgreement = {
      SearchableAgreement(
        id = domainModel.id.get,
        title = domainModel.title,
        content = domainModel.content,
        license = domainModel.copyright.license.get
      )
    }

    def getAgreementHits(response: JestSearchResult): Seq[api.AgreementSummary] = {
      response.getJsonObject.get("hits").asInstanceOf[JsonObject].get("hits").getAsJsonArray.asScala.map(jsonElem => {
        hitAsAgreementSummary(jsonElem.asInstanceOf[JsonObject].get("_source").asInstanceOf[JsonObject])
      }).toSeq
    }

    def hitAsAgreementSummary(hit: JsonObject): api.AgreementSummary = {
      val id = hit.get("id").getAsLong
      val title = hit.get("title").getAsString
      val license = hit.get("license").getAsString

      api.AgreementSummary(id,title, license)
    }

  }
}
