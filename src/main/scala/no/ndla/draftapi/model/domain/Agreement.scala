package no.ndla.draftapi.model.domain

import java.util.Date

import no.ndla.draftapi.DraftApiProperties
import org.json4s.FieldSerializer
import org.json4s.FieldSerializer._
import org.json4s.native.Serialization._
import scalikejdbc._

case class Agreement(
                    id: Option[Long],
                    content: String,
                    copyright: Copyright,
                    created: Date,
                    updated: Date,
                    updatedBy: String,
                    validFrom: Date,
                    validTo: Date)


object Agreement extends SQLSyntaxSupport[Agreement] {
  implicit val formats = org.json4s.DefaultFormats
  override val tableName = "agreementdata"
  override val schemaName = Some(DraftApiProperties.MetaSchema)

  def apply(lp: SyntaxProvider[Agreement])(rs:WrappedResultSet): Agreement = apply(lp.resultName)(rs)
  def apply(lp: ResultName[Agreement])(rs: WrappedResultSet): Agreement = {
    val meta = read[Agreement](rs.string(lp.c("document")))
    Agreement(
      Some(rs.long(lp.c("id"))),
      meta.content,
      meta.copyright,
      meta.created,
      meta.updated,
      meta.updatedBy,
      meta.validFrom,
      meta.validTo
    )
  }

  val JSonSerializer = FieldSerializer[Agreement](
    ignore("id")
  )
}
