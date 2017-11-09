/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.model.api.{Article => ApiArticleV2, ArticleContentV2}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.search._
import no.ndla.network.ApplicationUrl
import org.jsoup.Jsoup

trait SearchConverterService {
  val searchConverterService: SearchConverterService

  class SearchConverterService extends LazyLogging {
    def asSearchableAgreement(domainModel: Agreement): SearchableAgreement = {
      SearchableAgreement(
        id = domainModel.id.get,
        title = domainModel.title,
        content = domainModel.content,
        internalContactName = domainModel.internalContact.name,
        supplierContactName = domainModel.supplier.name
      )
    }

    def asSearchableArticle(ai: Article): SearchableArticle = {
      SearchableArticle(
        id = ai.id.get,
        title = SearchableLanguageValues(ai.title.map(title => LanguageValue(title.language, title.title))),
        visualElement = SearchableLanguageValues(ai.visualElement.map(visual => LanguageValue(visual.language, visual.resource))),
        introduction = SearchableLanguageValues(ai.introduction.map(intro => LanguageValue(intro.language, intro.introduction))),
        content = SearchableLanguageValues(ai.content.map(article => LanguageValue(article.language, Jsoup.parseBodyFragment(article.content).text()))),
        tags = SearchableLanguageList(ai.tags.map(tag => LanguageValue(tag.language, tag.tags))),
        lastUpdated = ai.updated,
        license = ai.copyright.license,
        authors = ai.copyright.creators.map(_.name) ++ ai.copyright.processors.map(_.name) ++ ai.copyright.rightsholders.map(_.name),
        articleType = ai.articleType
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

    def createUrlToArticle(id: Long): String = {
      s"${ApplicationUrl.get}$id"
    }

    def asSearchableConcept(c: Concept): SearchableConcept = {
      SearchableConcept(
        c.id.get,
        SearchableLanguageValues(c.title.map(title => LanguageValue(title.language, title.title))),
        SearchableLanguageValues(c.content.map(content => LanguageValue(content.language, content.content)))
      )
    }

  }
}
