/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import java.util.Date

import no.ndla.draftapi.DraftApiProperties
import org.joda.time.DateTime
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

case class Article(id: Option[Long],
                   revision: Option[Int],
                   status: Set[ArticleStatus.Value],
                   title: Seq[ArticleTitle],
                   content: Seq[ArticleContent],
                   copyright: Option[Copyright],
                   tags: Seq[ArticleTag],
                   requiredLibraries: Seq[RequiredLibrary],
                   visualElement: Seq[VisualElement],
                   introduction: Seq[ArticleIntroduction],
                   metaDescription: Seq[ArticleMetaDescription],
                   metaImageId: Option[String],
                   created: Date,
                   updated: Date,
                   updatedBy: String,
                   articleType: Option[ArticleType.Value]) extends Content


object Article extends SQLSyntaxSupport[Article] {
  implicit val formats = org.json4s.DefaultFormats + new EnumNameSerializer(ArticleStatus) + new EnumNameSerializer(ArticleType)
  override val tableName = "articledata"
  override val schemaName = Some(DraftApiProperties.MetaSchema)

  def apply(lp: SyntaxProvider[Article])(rs:WrappedResultSet): Article = apply(lp.resultName)(rs)
  def apply(lp: ResultName[Article])(rs: WrappedResultSet): Article = {
    val meta = read[Article](rs.string(lp.c("document")))
    Article(
      Some(rs.long(lp.c("id"))),
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
      meta.metaImageId,
      meta.created,
      meta.updated,
      meta.updatedBy,
      meta.articleType
    )
  }

  val JSonSerializer = FieldSerializer[Article](
    ignore("id") orElse
    ignore("revision")
  )
}

object ArticleStatus extends Enumeration {
  val CREATED, IMPORTED, USER_TEST, QUEUED_FOR_PUBLISHING, QUALITY_ASSURED, DRAFT, SKETCH, PUBLISHED = Value

  def valueOfOrError(s: String): Try[ArticleStatus.Value] =
    valueOf(s) match {
      case Some(st) => Success(st)
      case None =>
        val validStatuses = values.map(_.toString).mkString(", ")
        Failure(new ValidationException(errors=Seq(ValidationMessage("status", s"'$s' is not a valid article status. Must be one of $validStatuses"))))
    }

  def valueOf(s: String): Option[ArticleStatus.Value] = values.find(_.toString == s.toUpperCase)
}

object ArticleType extends Enumeration {
  val Standard = Value("standard")
  val TopicArticle = Value("topic-article")

  def all = ArticleType.values.map(_.toString).toSeq
  def valueOf(s:String): Option[ArticleType.Value] = ArticleType.values.find(_.toString == s)
  def valueOfOrError(s: String): ArticleType.Value =
    valueOf(s).getOrElse(throw new ValidationException(errors = List(ValidationMessage("articleType", s"'$s' is not a valid article type. Valid options are ${all.mkString(",")}."))))
}

case class Concept(id: Option[Long],
                   title: Seq[ConceptTitle],
                   content: Seq[ConceptContent],
                   copyright: Option[Copyright],
                   created: Date,
                   updated: Date) extends Content {
  lazy val supportedLanguages: Set[String] = (content union title).map(_.language).toSet
}

object Concept extends SQLSyntaxSupport[Concept] {
  implicit val formats = org.json4s.DefaultFormats
  override val tableName = "conceptdata"
  override val schemaName = Some(DraftApiProperties.MetaSchema)

  def apply(lp: SyntaxProvider[Concept])(rs:WrappedResultSet): Concept = apply(lp.resultName)(rs)
  def apply(lp: ResultName[Concept])(rs: WrappedResultSet): Concept = {
    val meta = read[Concept](rs.string(lp.c("document")))
    Concept(
      Some(rs.long(lp.c("id"))),
      meta.title,
      meta.content,
      meta.copyright,
      meta.created,
      meta.updated
    )
  }

  val JSonSerializer = FieldSerializer[Concept](
    ignore("id") orElse
      ignore("revision")
  )
}

case class Agreement(id: Option[Long],
                     title: String,
                     content: String,
                     copyright: Copyright,
                     created: Date,
                     updated: Date,
                     updatedBy: String) extends Content


object Agreement extends SQLSyntaxSupport[Agreement] {
  implicit val formats = org.json4s.DefaultFormats
  override val tableName = "agreementdata"
  override val schemaName = Some(DraftApiProperties.MetaSchema)

  def apply(lp: SyntaxProvider[Agreement])(rs:WrappedResultSet): Agreement = apply(lp.resultName)(rs)
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
