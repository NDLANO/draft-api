/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.integration.DataSource
import no.ndla.draftapi.model.api.{NotFoundException, OptimisticLockException}
import no.ndla.draftapi.model.domain.{Article, Concept}
import org.json4s.Formats
import org.postgresql.util.PGobject
import org.json4s.native.Serialization.write
import scalikejdbc._

import scala.util.{Failure, Success, Try}

trait ConceptRepository {
  this: DataSource =>
  val conceptRepository: ConceptRepository

  class ConceptRepository extends LazyLogging with Repository[Concept] {
    implicit val formats: Formats = org.json4s.DefaultFormats + Concept.JSonSerializer

    def insertWithExternalId(concept: Concept, externalId: String)(
        implicit session: DBSession = AutoSession): Concept = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(concept))

      val conceptId: Long =
        sql"""
        insert into ${Concept.table} (document, external_id) 
        values (${dataObject}, ARRAY[${externalId}]::text[])
          """.updateAndReturnGeneratedKey.apply

      logger.info(s"Inserted new concept: $conceptId")
      concept.copy(id = Some(conceptId))
    }

    def updateWithExternalId(concept: Concept, externalId: String)(
        implicit session: DBSession = AutoSession): Try[Concept] = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(concept))

      Try(sql"""
              update ${Concept.table} set document=${dataObject}
              where ${externalId} = any (external_id)
           """.updateAndReturnGeneratedKey.apply) match {
        case Success(id) => Success(concept.copy(id = Some(id)))
        case Failure(ex) =>
          logger.warn(s"Failed to update concept with external id $externalId: ${ex.getMessage}")
          Failure(ex)
      }
    }

    def newEmptyConcept(id: Long, externalId: List[String])(implicit session: DBSession = AutoSession): Try[Long] = {
      Try(sql"insert into ${Concept.table} (id, external_id) values ($id, ARRAY[$externalId]::text[])".update.apply) match {
        case Success(_) =>
          logger.info(s"Inserted new empty article: $id")
          Success(id)
        case Failure(ex) => Failure(ex)
      }
    }

    def update(concept: Concept)(implicit session: DBSession = AutoSession): Try[Concept] = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(concept))

      Try(
        sql"update ${Concept.table} set document=${dataObject} where id=${concept.id.get}".updateAndReturnGeneratedKey.apply) match {
        case Success(id) => Success(concept.copy(id = Some(id)))
        case Failure(ex) =>
          logger.warn(s"Failed to update concept with id ${concept.id}: ${ex.getMessage}")
          Failure(ex)
      }
    }

    def delete(conceptId: Long)(implicit session: DBSession = AutoSession): Try[Long] = {
      val numRows = sql"delete from ${Concept.table} where id = $conceptId".update().apply
      if (numRows == 1) {
        Success(conceptId)
      } else {
        Failure(NotFoundException(s"Concept with id $conceptId does not exist"))
      }
    }

    def withId(id: Long): Option[Concept] =
      conceptWhere(sqls"co.id=${id.toInt}")

    def exists(id: Long)(implicit session: DBSession = AutoSession): Boolean = {
      sql"select id from ${Concept.table} where id=${id}".map(rs => rs.long("id")).single.apply().isDefined
    }

    def getIdFromExternalId(externalId: String)(implicit session: DBSession = AutoSession): Option[Long] = {
      sql"select id from ${Concept.table} where $externalId = any(external_id)"
        .map(rs => rs.long("id"))
        .single
        .apply()
    }

    def withExternalId(externalId: String): Option[Concept] =
      conceptWhere(sqls"$externalId = any (co.external_id)")

    override def minMaxId(implicit session: DBSession = AutoSession): (Long, Long) = {
      sql"select coalesce(MIN(id),0) as mi, coalesce(MAX(id),0) as ma from ${Concept.table}"
        .map(rs => {
          (rs.long("mi"), rs.long("ma"))
        })
        .single()
        .apply() match {
        case Some(minmax) => minmax
        case None         => (0L, 0L)
      }
    }

    override def documentsWithIdBetween(min: Long, max: Long): List[Concept] =
      conceptsWhere(sqls"co.id between $min and $max")

    private def conceptWhere(whereClause: SQLSyntax)(
        implicit session: DBSession = ReadOnlyAutoSession): Option[Concept] = {
      val co = Concept.syntax("co")
      sql"select ${co.result.*} from ${Concept.as(co)} where co.document is not NULL and $whereClause"
        .map(Concept(co))
        .single
        .apply()
    }

    private def conceptsWhere(whereClause: SQLSyntax)(
        implicit session: DBSession = ReadOnlyAutoSession): List[Concept] = {
      val co = Concept.syntax("co")
      sql"select ${co.result.*} from ${Concept.as(co)} where co.document is not NULL and $whereClause"
        .map(Concept(co))
        .list
        .apply()
    }

  }
}
