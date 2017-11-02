/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class ArticleTag(tags: Seq[String],  language: String) extends LanguageField[Seq[String]] {
  override def value: Seq[String] = tags
}
