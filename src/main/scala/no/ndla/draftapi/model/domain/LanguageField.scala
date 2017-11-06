/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

trait WithLanguage {
  def language: String
}

trait LanguageField[T] extends WithLanguage {
  def value: T
  def isEmpty: Boolean
  def language: String
}
