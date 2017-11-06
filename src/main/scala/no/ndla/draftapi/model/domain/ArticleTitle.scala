/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class ArticleTitle(title: String, language: String) extends LanguageField[String] {
  override def value: String = title
  override def isEmpty: Boolean = value.isEmpty
}
