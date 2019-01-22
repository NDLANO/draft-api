/*
 * Part of NDLA draft-api.
 * Copyright (C) 2019 NDLA
 *
 * See LICENSE
 */

package db.migration

import java.util.Date
import java.util.UUID.randomUUID

import no.ndla.draftapi.model.domain._
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.JsonMethods.{compact, parse, render}
import org.json4s.{Extraction, Formats}
import org.jsoup.Jsoup
import org.jsoup.nodes.Entities.EscapeMode
import org.jsoup.nodes.TextNode
import org.postgresql.util.PGobject
import scalikejdbc.{DB, DBSession, _}

class V17__MoveTopicArticleEmbedToVisualElement extends BaseJavaMigration {
  override def migrate(context: Context): Unit = {
    val db = DB(context.getConnection)
    db.autoClose(false)

    db.withinTx { implicit session =>
      migrateArticleNotes
    }
  }

  def migrateArticleNotes(implicit session: DBSession): Unit = {
    val count = countAllArticles.get
    var numPagesLeft = (count / 1000) + 1
    var offset = 0L

    while (numPagesLeft > 0) {
      allArticles(offset * 1000).map {
        case (id, document) => updateArticle(convertTopicArticle(document), id)
      }
      numPagesLeft -= 1
      offset += 1
    }
  }

  def countAllArticles(implicit session: DBSession): Option[Long] = {
    sql"select count(*) from articledata where document is not NULL and document#>'{articleType}'='topic-article'"
      .map(rs => rs.long("count"))
      .single()
      .apply()
  }

  def allArticles(offset: Long)(implicit session: DBSession): Seq[(Long, String)] = {
    sql"select id, document from articledata where document is not null and document#>'{articleType}'='topic-article' order by id limit 1000 offset $offset"
      .map(rs => {
        (rs.long("id"), rs.string("document"))
      })
      .list
      .apply()
  }

  def convertTopicArticle(document: String): String = {
    implicit val formats
      : Formats = org.json4s.DefaultFormats + new EnumNameSerializer(ArticleStatus) + new EnumNameSerializer(
      ArticleType)

    val oldArticle = parse(document)
    val extractedArticle = oldArticle.extract[V16__Article]

    val contentWithExtractedEmbeds = extractedArticle.content.map(cont => {
      val (extractedEmbed, newContentString) = extractEmbedFromContent(cont.content)
      cont.copy(content = newContentString) -> extractedEmbed
    })

    val contentIsChanged = extractedArticle.content != contentWithExtractedEmbeds.map(_._1)

    val newVisualElements = contentWithExtractedEmbeds.collect {
      case (content, Some(extractedEmbed))
          if !extractedArticle.visualElement.map(_.language).contains(content.language) =>
        V16__VisualElement(extractedEmbed, content.language)
    }

    val allVisualElements = extractedArticle.visualElement ++ newVisualElements

    val updatedArticle = oldArticle.mapField {
      case ("visualElement", _) => "visualElement" -> Extraction.decompose(allVisualElements)
      case ("content", _)       => "content" -> Extraction.decompose(contentWithExtractedEmbeds.map(_._1))
      case ("notes", n) =>
        val existingNotes = n.extract[Seq[V16__EditorNote]]
        val noteToAppend =
          if (contentIsChanged) {
            Some(
              V16__EditorNote(
                s"Any embed before text has been deleted. Status changed to '${ArticleStatus.AWAITING_QUALITY_ASSURANCE}'.",
                "System",
                extractedArticle.status,
                new Date()
              ))
          } else { None }

        "notes" -> Extraction.decompose(existingNotes ++ noteToAppend)
      case x => x
    }

    compact(render(updatedArticle))
  }

  /** Returns a tuple with (extracted embed, new content body) */
  def extractEmbedFromContent(content: String): (Option[String], String) = {
    val document = Jsoup.parseBodyFragment(content)
    document.outputSettings().escapeMode(EscapeMode.xhtml).prettyPrint(false)
    val parsedContent = document.body()
    val firstEmbed = Option(parsedContent.select("embed").first())

    val willExtract = firstEmbed match {
      case Some(embed) =>
        val uuidStr = randomUUID().toString
        val textNode = new TextNode(uuidStr)
        embed.before(textNode)

        val shouldExtract = parsedContent.text().startsWith(uuidStr)
        textNode.remove()

        shouldExtract
      case None => false
    }

    if (willExtract) {
      val embedString = firstEmbed.map(e => {
        val html = e.outerHtml()
        e.remove()
        html
      })
      val ht = parsedContent.html()
      (embedString, ht)
    } else {
      (None, parsedContent.html())
    }
  }

  private def updateArticle(document: String, id: Long)(implicit session: DBSession): Int = {
    val dataObject = new PGobject()
    dataObject.setType("jsonb")
    dataObject.setValue(document)

    sql"update articledata set document = $dataObject where id = $id"
      .update()
      .apply
  }

  case class V16__Status(current: ArticleStatus.Value, other: Set[ArticleStatus.Value])
  case class V16__Content(content: String, language: String)
  case class V16__EditorNote(note: String, user: String, status: V16__Status, timestamp: Date)
  case class V16__VisualElement(resource: String, language: String)
  case class V16__Article(content: Seq[V16__Content],
                          visualElement: Seq[V16__VisualElement],
                          articleType: ArticleType.Value,
                          status: V16__Status)
}
