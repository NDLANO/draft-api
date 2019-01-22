/*
 * Part of NDLA draft-api.
 * Copyright (C) 2019 NDLA
 *
 * See LICENSE
 */

package db.migration

import java.util.Date

import no.ndla.draftapi.model.domain.{ArticleStatus, ArticleType}
import no.ndla.draftapi.{TestEnvironment, UnitSuite}
import org.json4s.Formats
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization.write

class V17__MoveTopicArticleEmbedToVisualElementTest extends UnitSuite with TestEnvironment {
  val migration = new V17__MoveTopicArticleEmbedToVisualElement

  val embed1 = """<embed data-resource=\"image\" data-resource_id=\"1\">"""
  val embed2 = """<embed data-resource=\"image\" data-resource_id=\"2\">"""
  val embed3 = """<embed data-resource=\"image\" data-resource_id=\"3\">"""
  val ve1 = s"""{"resource":"$embed1","language":"nb"}"""
  val ve2 = s"""{"resource":"$embed2","language":"nn"}"""
  val ve3 = s"""{"resource":"$embed3","language":"nn"}"""
  val co1 = """{"content":"<section><h1>No extract anything here brother</h1></section>","language":"nb"}"""
  val co2 = s"""{"content":"<section>$embed3<h1>Extract something here brother</h1></section>","language":"nn"}"""

  test("embed should be extracted if before all text") {
    val old =
      """<section><embed data-resource="image"><p>Hello Mister</p></section>"""

    val res = migration.extractEmbedFromContent(old)

    res._1.get should be("<embed data-resource=\"image\">")
    res._2 should be("<section><p>Hello Mister</p></section>")
  }

  test("embed should not be extracted if text appears before it") {
    val old =
      """<section><h1>Text is here</h1><embed data-resource="image"><p>Hello Mister</p></section>"""

    val res = migration.extractEmbedFromContent(old)

    res._1 should be(None)
    res._2 should be("<section><h1>Text is here</h1><embed data-resource=\"image\"><p>Hello Mister</p></section>")
  }

  test("Removed VisualElements should not be overwritten if extracted") {
    val coe1 = """{"content":"<section><h1>No extract anything here brother</h1></section>","language":"nb"}"""
    val coe2 = """{"content":"<section><h1>Extract something here brother</h1></section>","language":"nn"}"""
    val old =
      s"""{"articleType":"topic-article","visualElement":[$ve1,$ve2],"content":[$co1,$co2],"status":{"current":"PUBLISHED","other":[]}}"""

    val expected =
      s"""{"articleType":"topic-article","visualElement":[$ve1,$ve2],"content":[$coe1,$coe2],"status":{"current":"PUBLISHED","other":[]}}""" // TODO: Not published

    val res = migration.convertTopicArticle(old)

    res should be(expected)
  }

  test("Extracted embed should be used as visual element") {
    val coe1 = """{"content":"<section><h1>No extract anything here brother</h1></section>","language":"nb"}"""
    val coe2 = """{"content":"<section><h1>Extract something here brother</h1></section>","language":"nn"}"""

    val old =
      s"""{"articleType":"topic-article","visualElement":[$ve1],"content":[$co1,$co2],"status":{"current":"PUBLISHED","other":[]}}"""
    val expected =
      s"""{"articleType":"topic-article","visualElement":[$ve1,$ve3],"content":[$coe1,$coe2],"status":{"current":"PUBLISHED","other":[]}}""" // TODO: Not published

    val res = migration.convertTopicArticle(old)
    res should be(expected)
  }

  test("Status should be sucessfully updated to wait for quality assurance") { ??? }

  test("Notes should be added if embed is deleted") {
    implicit val formats
      : Formats = org.json4s.DefaultFormats + new EnumNameSerializer(ArticleStatus) + new EnumNameSerializer(
      ArticleType)

    val d = write(new Date())
    val existingNote = s"""{"note":"kake","user":"testleif","timestamp":$d,"status":{"current":"DRAFT","other":[]}}"""
    val noteText =
      s"Any embed before text has been deleted. Status changed to '${ArticleStatus.AWAITING_QUALITY_ASSURANCE}'."

    val old1 =
      s"""{"articleType":"topic-article","visualElement":[$ve1],"content":[$co1,$co2],"status":{"current":"PUBLISHED","other":[]},"notes":[$existingNote]}"""
    val old2 =
      s"""{"articleType":"topic-article","visualElement":[$ve1],"content":[$co1],"status":{"current":"PUBLISHED","other":[]},"notes":[]}"""

    val res1 = migration.convertTopicArticle(old1)
    val notes1 = (parse(res1) \ "notes").extract[Seq[migration.V16__EditorNote]]
    notes1.head.note should be("kake")
    notes1(1).note should be(noteText)
    notes1(1).user should be("System")
    notes1.size should be(2)

    val res2 = migration.convertTopicArticle(old2)
    val notes2 = (parse(res2) \ "notes").extract[Seq[migration.V16__EditorNote]]
    notes2.isEmpty should be(true)
  }
}
