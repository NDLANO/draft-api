/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.search

import java.util.Date

import no.ndla.draftapi.model.domain.emptySomeToNone
import no.ndla.draftapi.model.search.LanguageValue.{LanguageValue => LV}


object LanguageValue {

  case class LanguageValue[T](lang: String, value: T)

  def apply[T](lang: String, value: T): LanguageValue[T] = LanguageValue(lang, value)

}

case class SearchableLanguageValues(languageValues: Seq[LV[String]])

case class SearchableLanguageList(languageValues: Seq[LV[Seq[String]]])

case class SearchableArticle(
  id: Long,
  title: SearchableLanguageValues,
  content: SearchableLanguageValues,
  visualElement: SearchableLanguageValues,
  introduction: SearchableLanguageValues,
  tags: SearchableLanguageList,
  lastUpdated: Date,
  license: Option[String],
  authors: Seq[String],
  articleType: String,
  notes: Seq[String],
  defaultTitle: Option[String]
)

case class SearchableConcept(
  id: Long,
  title: SearchableLanguageValues,
  content: SearchableLanguageValues,
  defaultTitle: Option[String]
)
