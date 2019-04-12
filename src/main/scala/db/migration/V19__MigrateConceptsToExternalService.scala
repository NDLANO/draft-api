/*
 * Part of NDLA draft-api.
 * Copyright (C) 2019 NDLA
 *
 * See LICENSE
 */

package db.migration

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.model.domain.ArticleType
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}
import org.json4s.{DefaultFormats, Extraction, Formats, JString}
import org.json4s.ext.EnumNameSerializer
import scalikejdbc.{DB, DBSession}
import no.ndla.draftapi.model.domain.ArticleType
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}
import org.json4s.Extraction.decompose
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.JsonMethods.{compact, parse, render}
import org.jsoup.Jsoup
import org.jsoup.nodes.Entities.EscapeMode
import org.jsoup.nodes.{Element, TextNode}
import org.postgresql.util.PGobject
import scalaj.http.Http
import scalikejdbc.{DB, DBSession, _}

import scala.collection.JavaConverters._
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}

class V19__MigrateConceptsToExternalService extends BaseJavaMigration with LazyLogging {

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  private val explanationHost = s"${DraftApiProperties.ApiGatewayHost}"
  private val getConceptByExternalUrl = "/concepts/api/v1/Concept/GetByExternalId"

  override def migrate(context: Context): Unit = {
    val db = DB(context.getConnection)
    db.autoClose(false)

    db.withinTx { implicit session =>
      migrateArticleConcepts
    }
  }

  def migrateArticleConcepts(implicit session: DBSession): Unit = {
    val count = articleCount.get
    var numPagesLeft = (count / 1000) + 1
    var offset = 0L

    while (numPagesLeft > 0) {
      allArticles(offset * 1000).map {
        case (id, document) => updateArticle(convertArticle(document), id)
      }
      numPagesLeft -= 1
      offset += 1
    }
  }

  def articleCount(implicit session: DBSession): Option[Long] = {
    sql"""select count(*) from articledata where document is not NULL"""
      .map(rs => rs.long("count"))
      .single()
      .apply()
  }

  def allArticles(offset: Long)(implicit session: DBSession): Seq[(Long, String)] = {
    sql"""
         select id, document from articledata
         where document is not null
         order by id limit 1000 offset $offset
      """
      .map(rs => {
        (rs.long("id"), rs.string("document"))
      })
      .list
      .apply()
  }

  def getExplanationIdFromConceptId(oldConceptId: String): Option[Int] = {
    val idsTry = for {
      response <- Try(
        Http(s"$explanationHost$getConceptByExternalUrl")
          .param("externalId", oldConceptId)
          .timeout(5000, 5000)
          .asString)
      bodies <- Try(parse(response.body).asInstanceOf[JArray].arr)
      ids <- Try(bodies.map(data => (data \ "id").extract[Int]))
    } yield ids

    idsTry match {
      case Failure(ex) =>
        logger.error(s"Some error happened when fetching new concept id from old id of '$oldConceptId'", ex)
        None
      case Success(Nil) =>
        logger.error(s"No new concept id found for old concept id of $oldConceptId.")
        None
      case Success(head :: Nil) => Some(head)
      case Success(head :: tail) =>
        logger.warn(
          s"Found multiple new ids (${(head +: tail).mkString(", ")}) for old concept id of $oldConceptId. Picking $head.")
        Some(head)
    }
  }

  private def convertContent(content: List[V17_Content]): JValue = {
    val contents = content.map(cont => {
      val document = Jsoup.parseBodyFragment(cont.content)
      document
        .outputSettings()
        .escapeMode(EscapeMode.xhtml)
        .prettyPrint(false)
        .indentAmount(0)

      for (embed <- document.select("embed[data-resource='concept']").asScala) {
        Option(embed.attr("data-content-id")).foreach(oldId => {
          getExplanationIdFromConceptId(oldId)
            .foreach(newId => embed.attr("data-content-id", newId.toString))
        })
      }

      val newContentString = document.select("body").first().html()
      cont.copy(content = newContentString)
    })

    decompose(contents)
  }

  def convertArticle(document: String): String = {
    val oldArticle = parse(document)
    val newArticle = oldArticle.mapField {
      case ("content", content: JArray) => "content" -> convertContent(content.extract[List[V17_Content]])
      case field                        => field
    }
    compact(render(newArticle))
  }

  private def updateArticle(document: String, id: Long)(implicit session: DBSession): Int = {
    val dataObject = new PGobject()
    dataObject.setType("jsonb")
    dataObject.setValue(document)

    sql"update articledata set document = $dataObject where id = $id"
      .update()
      .apply
  }

  case class V17_Content(content: String, language: String)

}
