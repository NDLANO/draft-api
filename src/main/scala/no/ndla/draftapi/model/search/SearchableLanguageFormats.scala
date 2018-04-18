/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.search

import java.util.Date

import org.json4s.JsonAST.{JArray, JField, JObject, JString}
import org.json4s.{CustomSerializer, Extraction, Formats, MappingException}
import no.ndla.draftapi.model.domain.Language.UnknownLanguage

object LanguagelessSearchableArticle {

  case class LanguagelessSearchableArticle(
      id: Long,
      lastUpdated: Date,
      license: Option[String],
      authors: Seq[String],
      articleType: String,
      notes: Seq[String],
      defaultTitle: Option[String]
  )

  def apply(searchableArticle: SearchableArticle): LanguagelessSearchableArticle = {
    LanguagelessSearchableArticle(
      searchableArticle.id,
      searchableArticle.lastUpdated,
      searchableArticle.license,
      searchableArticle.authors,
      searchableArticle.articleType,
      searchableArticle.notes,
      searchableArticle.defaultTitle
    )
  }
}

object LanguagelessSearchableConcept {

  case class LanguagelessSearchableConcept(
      id: Long,
      defaultTitle: Option[String]
  )

  def apply(searchableConcept: SearchableConcept): LanguagelessSearchableConcept = {
    LanguagelessSearchableConcept(
      searchableConcept.id,
      searchableConcept.defaultTitle
    )
  }
}

class SearchableArticleSerializer
    extends CustomSerializer[SearchableArticle](_ =>
      ({
        case obj: JObject =>
          implicit val formats = org.json4s.DefaultFormats
          SearchableArticle(
            id = (obj \ "id").extract[Long],
            title = SearchableLanguageValues("title", obj),
            content = SearchableLanguageValues("content", obj),
            visualElement = SearchableLanguageValues("visualElement", obj),
            introduction = SearchableLanguageValues("introduction", obj),
            tags = SearchableLanguageList("tags", obj),
            lastUpdated = (obj \ "lastUpdated").extract[Date],
            license = (obj \ "license").extract[Option[String]],
            authors = (obj \ "authors").extract[Seq[String]],
            articleType = (obj \ "articleType").extract[String],
            notes = (obj \ "notes").extract[Seq[String]],
            defaultTitle = (obj \ "defaultTitle").extract[Option[String]]
          )
      }, {
        case article: SearchableArticle =>
          implicit val formats = org.json4s.DefaultFormats
          val languageFields =
            List(
              article.title.toJsonField("title"),
              article.content.toJsonField("content"),
              article.visualElement.toJsonField("visualElement"),
              article.introduction.toJsonField("introduction"),
              article.tags.toJsonField("tags")
            ).flatMap {
              case l: Seq[JField] => l
              case _              => Seq.empty
            }
          val partialSearchableArticle = LanguagelessSearchableArticle(article)
          val partialJObject = Extraction.decompose(partialSearchableArticle)
          partialJObject.merge(JObject(languageFields: _*))
      }))

class SearchableConceptSerializer
    extends CustomSerializer[SearchableConcept](_ =>
      ({
        case obj: JObject =>
          implicit val formats = org.json4s.DefaultFormats
          SearchableConcept(
            id = (obj \ "id").extract[Long],
            title = SearchableLanguageValues("title", obj),
            content = SearchableLanguageValues("content", obj),
            defaultTitle = (obj \ "defaultTitle").extract[Option[String]]
          )
      }, {
        case concept: SearchableConcept =>
          implicit val formats = org.json4s.DefaultFormats
          val languageFields =
            List(
              concept.title.toJsonField("title"),
              concept.content.toJsonField("content")
            ).flatMap {
              case l: Seq[JField] => l
              case _              => Seq.empty
            }

          val partialSearchableConcept = LanguagelessSearchableConcept(concept)
          val partialJObject = Extraction.decompose(partialSearchableConcept)
          partialJObject.merge(JObject(languageFields: _*))
      }))

object SearchableLanguageFormats {

  val JSonFormats: Formats =
    org.json4s.DefaultFormats +
      new SearchableArticleSerializer +
      new SearchableConceptSerializer
}
