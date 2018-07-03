/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */
package no.ndla.draftapi.model

package object domain {

  def emptySomeToNone(lang: Option[String]): Option[String] = {
    lang.filter(_.nonEmpty)
  }

  case class ArticleIds(articleId: Long, externalId: List[String])

  def getByLanguage[T](entries: Seq[LanguageField[T]], language: String): Option[T] =
    entries.find(_.language == language).map(_.value)
}
