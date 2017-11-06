/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi

import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.DraftApiProperties.resourceHtmlEmbedTag
import org.joda.time.{DateTime, DateTimeZone}

object TestData {
  private val publicDomainCopyright= Copyright(Some("publicdomain"), Some(""), List.empty)
  private val byNcSaCopyright = Copyright(Some("by-nc-sa"), Some("Gotham City"), List(Author("Forfatter", "DC Comics")))
  private val copyrighted = Copyright(Some("copyrighted"), Some("New York"), List(Author("Forfatter", "Clark Kent")))
  private val today = new DateTime().toDate

  private val embedUrl = "http://www.example.org"

  val (articleId, externalId) = (1, "751234")

  val sampleArticleV2 = api.Article(
    id=1,
    oldNdlaUrl = None,
    revision=1,
    title=Some(api.ArticleTitle("title", "nb")),
    content=Some(api.ArticleContent("this is content", "nb")),
    copyright = Some(api.Copyright(Some(api.License("licence", None, None)), Some("origin"), Seq(api.Author("developer", "Per")))),
    tags = Some(api.ArticleTag(Seq("tag"), "nb")),
    requiredLibraries = Seq(api.RequiredLibrary("JS", "JavaScript", "url")),
    visualElement = None,
    introduction = None,
    metaDescription = Some(api.ArticleMetaDescription("metaDesc", "nb")),
    created = new DateTime(2017, 1, 1, 12, 15, 32, DateTimeZone.UTC).toDate,
    updated = new DateTime(2017, 4, 1, 12, 15, 32, DateTimeZone.UTC).toDate,
    updatedBy = "me",
    articleType = Some("standard"),
    supportedLanguages = Seq("nb")
  )

  val articleHit1 = """
                      |{
                      |  "id": "4",
                      |  "title": [
                      |    {
                      |      "title": "8. mars, den internasjonale kvinnedagen",
                      |      "language": "nb"
                      |    },
                      |    {
                      |      "title": "8. mars, den internasjonale kvinnedagen",
                      |      "language": "nn"
                      |    }
                      |  ],
                      |  "introduction": [
                      |    {
                      |      "introduction": "8. mars er den internasjonale kvinnedagen.",
                      |      "language": "nb"
                      |    },
                      |    {
                      |      "introduction": "8. mars er den internasjonale kvinnedagen.",
                      |      "language": "nn"
                      |    }
                      |  ],
                      |  "url": "http://localhost:30002/article-api/v2/articles/4",
                      |  "license": "by-sa",
                      |  "articleType": "standard"
                      |}
                    """.stripMargin

  val apiArticleV2 = api.Article(
    articleId,
    Some(s"//red.ndla.no/node/$externalId"),
    2,
    Some(api.ArticleTitle("title", "nb")),
    Some(api.ArticleContent("content", "nb")),
    Some(api.Copyright(Some(api.License("by", Some("Creative Commons Attribution 2.0 Generic"), Some("https://creativecommons.org/licenses/by/2.0/"))), Some(""), Seq.empty)),
    None,
    Seq.empty,
    None,
    None,
    Some(api.ArticleMetaDescription("meta description", "nb")),
    today,
    today,
    "ndalId54321",
    Some("standard"),
    Seq("nb")
  )

  val sampleArticleWithPublicDomain = Article(
    Option(1),
    Option(1),
    Seq(ArticleTitle("test", "en")),
    Seq(ArticleContent("<section><div>test</div></section>", "en")),
    Some(publicDomainCopyright),
    Seq.empty,
    Seq.empty,
    Seq(VisualElement("image", "en")),
    Seq(ArticleIntroduction("This is an introduction", "en")),
    Seq.empty,
    None,
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    "ndalId54321",
    Some(ArticleType.Standard.toString))

  val sampleDomainArticle = Article(
    Option(articleId),
    Option(2),
    Seq(ArticleTitle("title", "nb")),
    Seq(ArticleContent("content", "nb")),
    Some(Copyright(Some("by"), Some(""), Seq.empty)),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq(ArticleMetaDescription("meta description", "nb")),
    None,
    today,
    today,
    "ndalId54321",
    Some(ArticleType.Standard.toString)
  )

  val sampleDomainArticle2 = Article(
    None,
    None,
    Seq(ArticleTitle("test", "en")),
    Seq(ArticleContent("<article><div>test</div></article>", "en")),
    Some(Copyright(Some("publicdomain"), Some(""), Seq.empty)),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    None,
    today,
    today,
    "ndalId54321",
    Some(ArticleType.Standard.toString)
  )

  val newArticle = api.NewArticle(
    "en",
    "test",
    Some("<article><div>test</div></article>"),
    Seq.empty,
    None,
    None,
    None,
    None,
    Some(api.Copyright(Some(api.License("publicdomain", None, None)), Some(""), Seq.empty)),
    Seq.empty,
    Some("standard")
  )

  val sampleArticleWithByNcSa = sampleArticleWithPublicDomain.copy(copyright=Some(byNcSaCopyright))
  val sampleArticleWithCopyrighted = sampleArticleWithPublicDomain.copy(copyright=Some(copyrighted))

  val sampleDomainArticleWithHtmlFault = Article(
    Option(articleId),
    Option(2),
    Seq(ArticleTitle("test", "en")),
    Seq(ArticleContent(
    """<ul><li><h1>Det er ikke lov å gjøre dette.</h1> Tekst utenfor.</li><li>Dette er helt ok</li></ul>
      |<ul><li><h2>Det er ikke lov å gjøre dette.</h2></li><li>Dette er helt ok</li></ul>
      |<ol><li><h3>Det er ikke lov å gjøre dette.</h3></li><li>Dette er helt ok</li></ol>
      |<ol><li><h4>Det er ikke lov å gjøre dette.</h4></li><li>Dette er helt ok</li></ol>
    """.stripMargin, "en")),
    Some(Copyright(Some("publicdomain"), Some(""), Seq.empty)),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq(ArticleMetaDescription("meta description", "nb")),
    None,
    today,
    today,
    "ndalId54321",
    Some(ArticleType.Standard.toString)
  )

  val apiArticleWithHtmlFaultV2 = api.Article(
    1,
    None,
    1,
    Some(api.ArticleTitle("test", "en")),
    Some(api.ArticleContent(
      """<ul><li><h1>Det er ikke lov å gjøre dette.</h1> Tekst utenfor.</li><li>Dette er helt ok</li></ul>
        |<ul><li><h2>Det er ikke lov å gjøre dette.</h2></li><li>Dette er helt ok</li></ul>
        |<ol><li><h3>Det er ikke lov å gjøre dette.</h3></li><li>Dette er helt ok</li></ol>
        |<ol><li><h4>Det er ikke lov å gjøre dette.</h4></li><li>Dette er helt ok</li></ol>
      """.stripMargin, "en")),
    Some(api.Copyright(Some(api.License("publicdomain", None, None)), Some(""), Seq.empty)),
    Some(api.ArticleTag(Seq.empty, "en")),
    Seq.empty,
    None,
    None,
    Some(api.ArticleMetaDescription("so meta", "en")),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    "ndalId54321",
    Some("standard"),
    Seq("en")
  )

  val (nodeId, nodeId2) = ("1234", "4321")
  val sampleTitle = ArticleTitle("title", "en")

  val visualElement = VisualElement(s"""<$resourceHtmlEmbedTag  data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1" data-size="" />""", "nb")

  val sampleConcept = Concept(
    Some(1),
    Seq(ConceptTitle("Tittel for begrep", "nb")),
    Seq(ConceptContent("Innhold for begrep", "nb")),
    Some(Copyright(Some("publicdomain"), Some(""), Seq.empty)),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate
  )

  val sampleApiConcept = api.Concept(
    1,
    api.ConceptTitle("Tittel for begrep", "nb"),
    api.ConceptContent("Innhold for begrep", "nb"),
    Some(api.Copyright(Some(api.License("publicdomain", None, None)), Some(""), Seq.empty)),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    Set("nb")
  )

}


