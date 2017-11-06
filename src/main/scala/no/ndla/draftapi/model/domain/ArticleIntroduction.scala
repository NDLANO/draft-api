/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class ArticleIntroduction(introduction: String, language: String) extends LanguageField[String] {
  override def value: String = introduction
  override def isEmpty: Boolean = value.isEmpty
}
