package no.ndla.draftapi.model.api

import com.scalatsi._
import com.scalatsi.dsl._

object TSTypes {
  implicit val author = TSType.fromCaseClass[Author]
  implicit val requiredLibrary = TSType.fromCaseClass[RequiredLibrary]
  implicit val editorNote = TSType.fromCaseClass[EditorNote]
  implicit val relatedContentLink = TSType.fromCaseClass[RelatedContentLink]

  implicit val x = TSType.fromCaseClass[NewArticleMetaImage] - "alt"
}
