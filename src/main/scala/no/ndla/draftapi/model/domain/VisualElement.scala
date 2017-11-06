/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.model.domain

case class VisualElement(resource: String, language: String) extends LanguageField[String] {
  override def value: String = resource
  override def isEmpty: Boolean = value.isEmpty
}
