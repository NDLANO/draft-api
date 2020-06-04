/*
 * Part of NDLA draft-api.
 * Copyright (C) 2020 NDLA
 *
 * See LICENSE
 */

package db.migration

import no.ndla.draftapi.DraftApiProperties
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}
import org.json4s
import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JArray, JObject, JString}
import org.json4s.native.JsonMethods.{compact, parse, render}
import org.postgresql.util.PGobject
import scalikejdbc.{DB, DBSession, _}

class V25__RemoveDomainFromH5PUrl extends BaseJavaMigration {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  override def migrate(context: Context) = {
    val db = DB(context.getConnection)
    db.autoClose(false)

    db.withinTx { implicit session =>
      migrateArticles
    }
  }

  def migrateArticles(implicit session: DBSession): Unit = {
    val count = countAllArticles.get
    var numPagesLeft = (count / 1000) + 1
    var offset = 0L
    val env = DraftApiProperties.Environment

    while (numPagesLeft > 0) {
      allArticles(offset * 1000).map {
        case (id, document) => updateArticle(convertArticleUpdate(document, env), id)
      }
      numPagesLeft -= 1
      offset += 1
    }
  }

  def countAllArticles(implicit session: DBSession): Option[Long] = {
    sql"select count(*) from articledata where document is not NULL"
      .map(rs => rs.long("count"))
      .single()
      .apply()
  }

  def allArticles(offset: Long)(implicit session: DBSession): Seq[(Long, String)] = {
    sql"select id, document, article_id from articledata where document is not null order by id limit 1000 offset $offset"
      .map(rs => {
        (rs.long("id"), rs.string("document"))
      })
      .list
      .apply()
  }

  def updateArticle(document: String, id: Long)(implicit session: DBSession): Int = {
    val dataObject = new PGobject()
    dataObject.setType("jsonb")
    dataObject.setValue(document)

    sql"update articledata set document = $dataObject where id = $id"
      .update()
      .apply
  }

  def updateH5PDomains(html: String, env: String): String = {
    val oldDomain = if (env != "prod") s"https://h5p-${env}.ndla.no" else "https://h5p.ndla.no"
    val newDomain = ""

    val updatedHtml = html.replaceAll(oldDomain, newDomain)
    updateH5PResourceType(updatedHtml)
  }

  def updateH5PResourceType(html: String): String = {
    val oldResourceType = "external"
    val newResourceType = "h5p"

    html.replaceAll(oldResourceType, newResourceType)
  }

  def updateContent(contents: JArray, contentType: String, env: String): json4s.JValue = {
    contents.map {
      case content =>
        content.mapField {
          case (`contentType`, JString(html)) => (`contentType`, JString(updateH5PDomains(html, env)))
          case z                              => z
        }
      case y => y
    }
  }

  private[migration] def convertArticleUpdate(document: String, env: String): String = {
    val oldArticle = parse(document)

    val newArticle = oldArticle.mapField {
      case ("visualElement", visualElements: JArray) => {
        val updatedContent = updateContent(visualElements, "resource", env)
        ("visualElement", updatedContent)
      }
      case ("content", contents: JArray) => {
        val updatedContent = updateContent(contents, "content", env)
        ("content", updatedContent)
      }
      case x => x
    }

    compact(render(newArticle))
  }

}
