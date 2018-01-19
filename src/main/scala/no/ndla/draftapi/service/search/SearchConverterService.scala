/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import com.sksamuel.elastic4s.http.search.SearchHit
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.model.search._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.network.ApplicationUrl
import org.jsoup.Jsoup
import org.json4s._
import org.json4s.native.JsonMethods._

import scala.collection.JavaConverters._
import no.ndla.draftapi.model.domain.Language._
import no.ndla.draftapi.service.ConverterService

trait SearchConverterService {
  this: ConverterService =>
  val searchConverterService: SearchConverterService

  class SearchConverterService extends LazyLogging {
    implicit val formats = DefaultFormats
    def asSearchableArticle(ai: Article): SearchableArticle = {

      val defaultTitle = ai.title.sortBy(title => {
        val languagePriority = Language.languageAnalyzers.map(la => la.lang).reverse
        languagePriority.indexOf(title.language)
      }).lastOption

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
        articleType = ai.articleType.toString,
        notes = ai.notes,
        defaultTitle = defaultTitle.map(_.title)
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
      val defaultTitle = c.title.sortBy(title => {
        val languagePriority = Language.languageAnalyzers.map(la => la.lang).reverse
        languagePriority.indexOf(title.language)
      }).lastOption

      SearchableConcept(
        id = c.id.get,
        title = SearchableLanguageValues(c.title.map(title => LanguageValue(title.language, title.title))),
        content = SearchableLanguageValues(c.content.map(content => LanguageValue(content.language, content.content))),
        defaultTitle = defaultTitle.map(_.title)
      )
    }

    def hitAsArticleSummary(hitString: String, language: String): api.ArticleSummary = {
      val hit = parse(hitString)
      val titles = (hit \ "title").extract[Map[String, String]].map(title => domain.ArticleTitle(title._2, title._1)).toSeq
      val introductions = (hit \ "introduction").extract[Map[String, String]].map(intro => domain.ArticleIntroduction(intro._2, intro._1)).toSeq
      val visualElements = (hit \ "visualElement").extract[Map[String, String]].map(visual => domain.VisualElement(visual._2, visual._1)).toSeq

      val notes = (hit \ "notes").extract[Seq[String]]
      val supportedLanguages = getSupportedLanguages(Seq(titles, visualElements, introductions))

      val title = findByLanguageOrBestEffort(titles, language).map(converterService.toApiArticleTitle).getOrElse(api.ArticleTitle("", DefaultLanguage))
      val visualElement = findByLanguageOrBestEffort(visualElements, language).map(converterService.toApiVisualElement)
      val introduction = findByLanguageOrBestEffort(introductions, language).map(converterService.toApiArticleIntroduction)

      api.ArticleSummary(
        (hit \ "id").extract[Long],
        title,
        visualElement,
        introduction,
        ApplicationUrl.get + (hit \ "id").extract[Long],
        (hit \ "license").extract[String],
        (hit \ "articleType").extract[String],
        supportedLanguages,
        notes
      )
    }


    def hitAsConceptSummary(hitString: String, language: String): api.ConceptSummary = {
      val hit = parse(hitString)
      val titles = (hit \ "title").extract[Map[String, String]].map(title => domain.ConceptTitle(title._2, title._1)).toSeq
      val contents = (hit \ "content").extract[Map[String, String]].map(content => domain.ConceptContent(content._2, content._1)).toSeq

      val supportedLanguages = (titles union contents).map(_.language).toSet

      val title = Language.findByLanguageOrBestEffort(titles, language).map(converterService.toApiConceptTitle).getOrElse(api.ConceptTitle("", Language.DefaultLanguage))
      val concept = Language.findByLanguageOrBestEffort(contents, language).map(converterService.toApiConceptContent).getOrElse(api.ConceptContent("", Language.DefaultLanguage))

      api.ConceptSummary(
        (hit \ "id").extract[Long],
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

      api.AgreementSummary(id,title, license)
    }

    def getLanguageFromHit(result: SearchHit): Option[String] = {
      val sortedInnerHits = result.innerHits.toList.filter(ih => ih._2.total > 0).sortBy{
        case (_, hit) => hit.max_score
      }.reverse

      val matchLanguage = sortedInnerHits.headOption.flatMap{
        case (_, innerHit) =>
          innerHit.hits.sortBy(hit => hit.score).reverse.headOption.flatMap(hit => {
            hit.highlight.headOption.map(hl => {
                hl._1.split('.').filterNot(_ == "raw").last
            })
          })
      }

      matchLanguage match {
        case Some(lang) =>
          Some(lang)
        case _ =>
          val title = result.sourceAsMap.get("title")
          val titleMap = title.map(tm => {
            tm.asInstanceOf[Map[String, _]]
          })

          val languages = titleMap.map(title => title.keySet.toList)

          languages.flatMap(languageList => {
            languageList.sortBy(lang => {
              val languagePriority = Language.languageAnalyzers.map(la => la.lang).reverse
              languagePriority.indexOf(lang)
            }).lastOption
          })
      }
    }

  }
}
