/*
 * Part of NDLA draft_api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.search

import no.ndla.draftapi.model.domain.LanguageField

case class LanguageValue[T](language: String, value: T) extends LanguageField[T] {
  override def isEmpty: Boolean = false
}

case class SearchableLanguageValues(languageValues: Seq[LanguageValue[String]])

object SearchableLanguageValues {

  def fieldsToSearchableLanguageValues[T <: LanguageField[String]](fields: Seq[T]): SearchableLanguageValues = {
    SearchableLanguageValues(fields.map(f => LanguageValue(f.language, f.value)))
  }
}

case class SearchableLanguageList(languageValues: Seq[LanguageValue[Seq[String]]])
