/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.integration.DataSource
import no.ndla.draftapi.model.domain.Agreement
import org.json4s.Formats
import org.json4s.native.Serialization.write
import org.postgresql.util.PGobject
import scalikejdbc._

import scala.util.{Failure, Success, Try}

trait AgreementRepository {
  this: DataSource =>
  val agreementRepository: AgreementRepository

  class AgreementRepository extends LazyLogging with Repository[Agreement] {
    implicit val formats: Formats = org.json4s.DefaultFormats + Agreement.JSonSerializer

    def withId(id: Long): Option[Agreement] =
      agreementWhere(sqls"co.id=${id.toInt}")

    def withExternalId(externalId: String): Option[Concept] =
      conceptWhere(sqls"co.external_id=$externalId")

    def exists(externalId: String): Boolean =
      conceptWhere(sqls"co.external_id=$externalId").isDefined

    override def minMaxId(implicit session: DBSession = AutoSession): (Long, Long) = {
      sql"select coalesce(MIN(id),0) as mi, coalesce(MAX(id),0) as ma from ${Concept.table}".map(rs => {
        (rs.long("mi"), rs.long("ma"))
      }).single().apply() match {
        case Some(minmax) => minmax
        case None => (0L, 0L)
      }
    }

    override def documentsWithIdBetween(min: Long, max: Long): List[Concept] =
      conceptsWhere(sqls"co.id between $min and $max")

    private def agreementWhere(whereClause: SQLSyntax)(implicit session: DBSession = ReadOnlyAutoSession): Option[Concept] = {
      val co = Agreement.syntax("co")
      sql"select ${co.result.*} from ${Concept.as(co)} where $whereClause".map(Concept(co)).single.apply()
    }

    private def conceptsWhere(whereClause: SQLSyntax)(implicit session: DBSession = ReadOnlyAutoSession): List[Concept] = {
      val co = Concept.syntax("co")
      sql"select ${co.result.*} from ${Concept.as(co)} where $whereClause".map(Concept(co)).list.apply()
    }

  }
}
