/*
 * Part of NDLA draft_api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class ArticleMetaImage(imageId: String, language: String) extends LanguageField[String] {
  override def value: String = imageId
  override def isEmpty: Boolean = value.isEmpty
}
