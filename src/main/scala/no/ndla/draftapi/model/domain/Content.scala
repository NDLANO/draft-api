/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import java.util.Date

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.model.domain.Language.getSupportedLanguages
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.json4s.FieldSerializer
import org.json4s.FieldSerializer._
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.Serialization._
import scalikejdbc._

import scala.util.{Failure, Success, Try}

sealed trait Content {
  def id: Option[Long]
}

case class Article(
    id: Option[Long],
    revision: Option[Int],
    status: Status,
    title: Seq[ArticleTitle],
    content: Seq[ArticleContent],
    copyright: Option[Copyright],
    tags: Seq[ArticleTag],
    requiredLibraries: Seq[RequiredLibrary],
    visualElement: Seq[VisualElement],
    introduction: Seq[ArticleIntroduction],
    metaDescription: Seq[ArticleMetaDescription],
    metaImage: Seq[ArticleMetaImage],
    created: Date,
    updated: Date,
    updatedBy: String,
    published: Date,
    articleType: ArticleType.Value,
    notes: Seq[EditorNote],
    previousVersionsNotes: Seq[EditorNote]
) extends Content {

  def supportedLanguages =
    getSupportedLanguages(Seq(title, visualElement, introduction, metaDescription, tags, content, metaImage))
}

object Article extends SQLSyntaxSupport[Article] {
  implicit val formats = org.json4s.DefaultFormats + new EnumNameSerializer(ArticleStatus) + new EnumNameSerializer(
    ArticleType)
  override val tableName = "articledata"
  override val schemaName = Some(DraftApiProperties.MetaSchema)

  def apply(lp: SyntaxProvider[Article])(rs: WrappedResultSet): Article = apply(lp.resultName)(rs)

  def apply(lp: ResultName[Article])(rs: WrappedResultSet): Article = {
    val meta = read[Article](rs.string(lp.c("document")))
    Article(
      Some(rs.long(lp.c("article_id"))),
      Some(rs.int(lp.c("revision"))),
      meta.status,
      meta.title,
      meta.content,
      meta.copyright,
      meta.tags,
      meta.requiredLibraries,
      meta.visualElement,
      meta.introduction,
      meta.metaDescription,
      meta.metaImage,
      meta.created,
      meta.updated,
      meta.updatedBy,
      meta.published,
      meta.articleType,
      meta.notes,
      meta.previousVersionsNotes
    )
  }

  val JSonSerializer = FieldSerializer[Article](
    ignore("id") orElse
      ignore("revision")
  )
}

object ArticleStatusAction extends Enumeration {
  val UPDATE = Value
}

object ArticleStatus extends Enumeration {

  val IMPORTED, DRAFT, PUBLISHED, PROPOSAL, QUEUED_FOR_PUBLISHING, USER_TEST, AWAITING_QUALITY_ASSURANCE,
  QUEUED_FOR_LANGUAGE, TRANSLATED, QUALITY_ASSURED, AWAITING_UNPUBLISHING, UNPUBLISHED, ARCHIVED = Value

  def valueOfOrError(s: String): Try[ArticleStatus.Value] =
    valueOf(s) match {
      case Some(st) => Success(st)
      case None =>
        val validStatuses = values.map(_.toString).mkString(", ")
        Failure(
          new ValidationException(
            errors =
              Seq(ValidationMessage("status", s"'$s' is not a valid article status. Must be one of $validStatuses"))))
    }

  def valueOf(s: String): Option[ArticleStatus.Value] = values.find(_.toString == s.toUpperCase)
}

object ArticleType extends Enumeration {
  val Standard = Value("standard")
  val TopicArticle = Value("topic-article")

  def all = ArticleType.values.map(_.toString).toSeq
  def valueOf(s: String): Option[ArticleType.Value] = ArticleType.values.find(_.toString == s)

  def valueOfOrError(s: String): ArticleType.Value =
    valueOf(s).getOrElse(throw new ValidationException(errors = List(
      ValidationMessage("articleType", s"'$s' is not a valid article type. Valid options are ${all.mkString(",")}."))))
}

case class Agreement(
    id: Option[Long],
    title: String,
    content: String,
    copyright: Copyright,
    created: Date,
    updated: Date,
    updatedBy: String
) extends Content

object Agreement extends SQLSyntaxSupport[Agreement] {
  implicit val formats = org.json4s.DefaultFormats
  override val tableName = "agreementdata"
  override val schemaName = Some(DraftApiProperties.MetaSchema)

  def apply(lp: SyntaxProvider[Agreement])(rs: WrappedResultSet): Agreement = apply(lp.resultName)(rs)

  def apply(lp: ResultName[Agreement])(rs: WrappedResultSet): Agreement = {
    val meta = read[Agreement](rs.string(lp.c("document")))
    Agreement(
      Some(rs.long(lp.c("id"))),
      meta.title,
      meta.content,
      meta.copyright,
      meta.created,
      meta.updated,
      meta.updatedBy
    )
  }

  val JSonSerializer = FieldSerializer[Agreement](
    ignore("id")
  )
}
