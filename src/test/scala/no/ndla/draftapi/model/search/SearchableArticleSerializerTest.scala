package no.ndla.draftapi.model.search

import no.ndla.draftapi.{TestEnvironment, UnitSuite}
import org.joda.time.{DateTime, DateTimeZone}
import org.json4s.Formats
import org.json4s.native.Serialization.{read, writePretty}

class SearchableArticleSerializerTest extends UnitSuite with TestEnvironment {
  implicit val formats: Formats = SearchableLanguageFormats.JSonFormats

  val searchableArticle1 = SearchableArticle(
    id = 10.toLong,
    title = SearchableLanguageValues(Seq(LanguageValue("nb", "tittel"), LanguageValue("en", "title"))),
    content = SearchableLanguageValues(Seq(LanguageValue("nb", "innhold"), LanguageValue("en", "content"))),
    visualElement = SearchableLanguageValues(Seq(LanguageValue("nb", "visueltelement"), LanguageValue("en", "visualelement"))),
    introduction = SearchableLanguageValues(List(LanguageValue("nb", "ingress"), LanguageValue("en", "introduction"))),
    tags = SearchableLanguageList(List(LanguageValue("nb", List("m", "e", "r", "k")), LanguageValue("en", List("t", "a", "g", "s")))),
    lastUpdated = new DateTime(2018, 2, 22, 13, 0, 51, DateTimeZone.UTC).toDate,
    license = Some("by-sa"),
    authors = Seq("Jonas Natty"),
    notes = Seq("jak"),
    articleType = "standard",
    defaultTitle = Some("tjuppidu")
  )

  val searchableArticle1AsJson =
    """{
      |  "id":10,
      |  "lastUpdated":"2018-02-22T13:00:51Z",
      |  "license":"by-sa",
      |  "authors":[
      |    "Jonas Natty"
      |  ],
      |  "articleType":"standard",
      |  "notes":[
      |    "jak"
      |  ],
      |  "defaultTitle":"tjuppidu",
      |  "title.nb":"tittel",
      |  "title.en":"title",
      |  "content.nb":"innhold",
      |  "content.en":"content",
      |  "visualElement.nb":"visueltelement",
      |  "visualElement.en":"visualelement",
      |  "introduction.nb":"ingress",
      |  "introduction.en":"introduction",
      |  "tags.nb":[
      |    "m",
      |    "e",
      |    "r",
      |    "k"
      |  ],
      |  "tags.en":[
      |    "t",
      |    "a",
      |    "g",
      |    "s"
      |  ]
      |}""".stripMargin

  val searchableConcept1 = SearchableConcept(
    id = 5.toLong,
    title = SearchableLanguageValues(Seq(LanguageValue("nb", "tittel"), LanguageValue("en", "title"))),
    content = SearchableLanguageValues(Seq(LanguageValue("nb", "innhold"), LanguageValue("en", "content"))),
    defaultTitle = Some("tjappsipappsi")
  )

  val searchableConcept1AsJson =
    """{
      |  "id":5,
      |  "defaultTitle":"tjappsipappsi",
      |  "title.nb":"tittel",
      |  "title.en":"title",
      |  "content.nb":"innhold",
      |  "content.en":"content"
      |}""".stripMargin


  test("That serialization of SearchableArticle works correctly") {
    val json = writePretty(searchableArticle1)
    json.replace("\n", "") should be(searchableArticle1AsJson.replace("\n", ""))
  }

  test("That deserialization of SearchableArticle works correctly") {
    val parsedSearchableArticle = read[SearchableArticle](searchableArticle1AsJson)
    parsedSearchableArticle should be(searchableArticle1)
  }

  test("That serialization of SearchableConcept works correctly") {
    val json = writePretty(searchableConcept1)
    json.replace("\n", "") should be(searchableConcept1AsJson.replace("\n", ""))
  }

  test("That deserialization of SearchableConcept works correctly") {
    val parsedSearchableArticle = read[SearchableConcept](searchableConcept1AsJson)
    parsedSearchableArticle should be(searchableConcept1)
  }

}
