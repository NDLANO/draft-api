/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi

import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.DraftApiProperties.resourceHtmlEmbedTag
import ArticleStatus._
import no.ndla.draftapi.auth.{Role, UserInfo}
import no.ndla.draftapi.integration.{EmbedUrl, LearningPath, LearningStep}
import no.ndla.draftapi.model.api.NewAgreement
import org.joda.time.{DateTime, DateTimeZone}
import no.ndla.mapping.License.{CC_BY_NC_SA, CC_BY, CC_BY_SA}

object TestData {

  val authHeaderWithWriteRole =
    "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoiZHJhZnRzLXRlc3Q6d3JpdGUiLCJndHkiOiJjbGllbnQtY3JlZGVudGlhbHMifQ.i_wvbN24VZMqOTQPiEqvqKZy23-m-2ZxTligof8n33k3z-BjXqn4bhKTv7sFdQG9Wf9TFx8UzjoOQ6efQgpbRzl8blZ-6jAZOy6xDjDW0dIwE0zWD8riG8l27iQ88fbY_uCyIODyYp2JNbVmWZNJ9crKKevKmhcXvMRUTrcyE9g"

  val authHeaderWithoutAnyRoles =
    "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoiIiwiZ3R5IjoiY2xpZW50LWNyZWRlbnRpYWxzIn0.fb9eTuBwIlbGDgDKBQ5FVpuSUdgDVBZjCenkOrWLzUByVCcaFhbFU8CVTWWKhKJqt6u-09-99hh86szURLqwl3F5rxSX9PrnbyhI9LsPut_3fr6vezs6592jPJRbdBz3-xLN0XY5HIiJElJD3Wb52obTqJCrMAKLZ5x_GLKGhcY"

  val authHeaderWithWrongRole =
    "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoic29tZTpvdGhlciIsImd0eSI6ImNsaWVudC1jcmVkZW50aWFscyJ9.Hbmh9KX19nx7yT3rEcP9pyzRO0uQJBRucfqH9QEZtLyXjYj_fAyOhsoicOVEbHSES7rtdiJK43-gijSpWWmGWOkE6Ym7nHGhB_nLdvp_25PDgdKHo-KawZdAyIcJFr5_t3CJ2Z2IPVbrXwUd99vuXEBaV0dMwkT0kDtkwHuS-8E"

  val authHeaderWithAllRoles =
    "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6Ik9FSTFNVVU0T0RrNU56TTVNekkyTXpaRE9EazFOMFl3UXpkRE1EUXlPRFZDUXpRM1FUSTBNQSJ9.eyJodHRwczovL25kbGEubm8vY2xpZW50X2lkIjoieHh4eXl5IiwiaXNzIjoiaHR0cHM6Ly9uZGxhLmV1LmF1dGgwLmNvbS8iLCJzdWIiOiJ4eHh5eXlAY2xpZW50cyIsImF1ZCI6Im5kbGFfc3lzdGVtIiwiaWF0IjoxNTEwMzA1NzczLCJleHAiOjE1MTAzOTIxNzMsInNjb3BlIjoiYXJ0aWNsZXMtdGVzdDpwdWJsaXNoIGRyYWZ0cy10ZXN0OndyaXRlIGRyYWZ0cy10ZXN0OnNldF90b19wdWJsaXNoIiwiZ3R5IjoiY2xpZW50LWNyZWRlbnRpYWxzIn0.gsM-U84ykgaxMSbL55w6UYIIQUouPIB6YOmJuj1KhLFnrYctu5vwYBo80zyr1je9kO_6L-rI7SUnrHVao9DFBZJmfFfeojTxIT3CE58hoCdxZQZdPUGePjQzROWRWeDfG96iqhRcepjbVF9pMhKp6FNqEVOxkX00RZg9vFT8iMM"

  val userWithNoRoles = UserInfo("unit test", Set.empty)
  val userWithWriteAccess = UserInfo("unit test", Set(Role.WRITE))
  val userWithPublishAccess = UserInfo("unit test", Set(Role.WRITE, Role.SET_TO_PUBLISH))
  val userWIthAdminAccess = UserInfo("unit test", Set(Role.WRITE, Role.SET_TO_PUBLISH, Role.ADMIN))

  private val publicDomainCopyright =
    Copyright(Some("publicdomain"), Some(""), List.empty, List(), List(), None, None, None)
  private val byNcSaCopyright = Copyright(Some(CC_BY_NC_SA.toString),
                                          Some("Gotham City"),
                                          List(Author("Forfatter", "DC Comics")),
                                          List(),
                                          List(),
                                          None,
                                          None,
                                          None)
  private val copyrighted = Copyright(Some("copyrighted"),
                                      Some("New York"),
                                      List(Author("Forfatter", "Clark Kent")),
                                      List(),
                                      List(),
                                      None,
                                      None,
                                      None)
  val today = new DateTime().toDate

  private val embedUrl = "http://www.example.org"

  val (articleId, externalId) = (1, "751234")

  val sampleArticleV2 = api.Article(
    id = 1,
    oldNdlaUrl = None,
    revision = 1,
    status = api.Status(DRAFT.toString, Seq.empty),
    title = Some(api.ArticleTitle("title", "nb")),
    content = Some(api.ArticleContent("this is content", "nb")),
    copyright = Some(
      api.Copyright(Some(api.License("licence", None, None)),
                    Some("origin"),
                    Seq(api.Author("developer", "Per")),
                    List(),
                    List(),
                    None,
                    None,
                    None)),
    tags = Some(api.ArticleTag(Seq("tag"), "nb")),
    requiredLibraries = Seq(api.RequiredLibrary("JS", "JavaScript", "url")),
    visualElement = None,
    introduction = None,
    metaDescription = Some(api.ArticleMetaDescription("metaDesc", "nb")),
    None,
    created = new DateTime(2017, 1, 1, 12, 15, 32, DateTimeZone.UTC).toDate,
    updated = new DateTime(2017, 4, 1, 12, 15, 32, DateTimeZone.UTC).toDate,
    updatedBy = "me",
    articleType = "standard",
    supportedLanguages = Seq("nb"),
    Seq.empty
  )

  val sampleApiUpdateArticle = api.UpdatedArticle(
    1,
    Some("nb"),
    Some("tittel"),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None
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
    api.Status(DRAFT.toString, Seq.empty),
    Some(api.ArticleTitle("title", "nb")),
    Some(api.ArticleContent("content", "nb")),
    Some(
      api.Copyright(
        Some(
          api.License(CC_BY.toString,
                      Some("Creative Commons Attribution 4.0 International"),
                      Some("https://creativecommons.org/licenses/by/4.0/"))),
        Some(""),
        Seq.empty,
        List(),
        List(),
        None,
        None,
        None
      )),
    None,
    Seq.empty,
    None,
    None,
    Some(api.ArticleMetaDescription("meta description", "nb")),
    None,
    today,
    today,
    "ndalId54321",
    "standard",
    Seq("nb"),
    Seq.empty
  )

  val sampleArticleWithPublicDomain = Article(
    Option(1),
    Option(1),
    domain.Status(DRAFT, Set.empty),
    Seq(ArticleTitle("test", "en")),
    Seq(ArticleContent("<section><div>test</div></section>", "en")),
    Some(publicDomainCopyright),
    Seq.empty,
    Seq.empty,
    Seq(VisualElement("image", "en")),
    Seq(ArticleIntroduction("This is an introduction", "en")),
    Seq.empty,
    Seq.empty,
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    userWithWriteAccess.id,
    ArticleType.Standard,
    Seq.empty
  )

  val sampleDomainArticle = Article(
    Option(articleId),
    Option(2),
    domain.Status(DRAFT, Set.empty),
    Seq(ArticleTitle("title", "nb")),
    Seq(ArticleContent("content", "nb")),
    Some(Copyright(Some(CC_BY.toString), Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None)),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq(ArticleMetaDescription("meta description", "nb")),
    Seq.empty,
    today,
    today,
    "ndalId54321",
    ArticleType.Standard,
    Seq.empty
  )

  val sampleDomainArticle2 = Article(
    None,
    None,
    domain.Status(DRAFT, Set.empty),
    Seq(ArticleTitle("test", "en")),
    Seq(ArticleContent("<article><div>test</div></article>", "en")),
    Some(Copyright(Some("publicdomain"), Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None)),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    today,
    today,
    "ndalId54321",
    ArticleType.Standard,
    Seq.empty
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
    Some(
      api.Copyright(Some(api.License("publicdomain", None, None)),
                    Some(""),
                    Seq.empty,
                    Seq.empty,
                    Seq.empty,
                    None,
                    None,
                    None)),
    Seq.empty,
    "standard",
    Seq.empty
  )

  val sampleArticleWithByNcSa = sampleArticleWithPublicDomain.copy(copyright = Some(byNcSaCopyright))
  val sampleArticleWithCopyrighted = sampleArticleWithPublicDomain.copy(copyright = Some(copyrighted))

  val sampleDomainArticleWithHtmlFault = Article(
    Option(articleId),
    Option(2),
    domain.Status(DRAFT, Set.empty),
    Seq(ArticleTitle("test", "en")),
    Seq(
      ArticleContent(
        """<ul><li><h1>Det er ikke lov å gjøre dette.</h1> Tekst utenfor.</li><li>Dette er helt ok</li></ul>
      |<ul><li><h2>Det er ikke lov å gjøre dette.</h2></li><li>Dette er helt ok</li></ul>
      |<ol><li><h3>Det er ikke lov å gjøre dette.</h3></li><li>Dette er helt ok</li></ol>
      |<ol><li><h4>Det er ikke lov å gjøre dette.</h4></li><li>Dette er helt ok</li></ol>
    """.stripMargin,
        "en"
      )),
    Some(Copyright(Some("publicdomain"), Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None)),
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq(ArticleMetaDescription("meta description", "nb")),
    Seq.empty,
    today,
    today,
    "ndalId54321",
    ArticleType.Standard,
    Seq.empty
  )

  val apiArticleWithHtmlFaultV2 = api.Article(
    1,
    None,
    1,
    api.Status(DRAFT.toString, Seq.empty),
    Some(api.ArticleTitle("test", "en")),
    Some(
      api.ArticleContent(
        """<ul><li><h1>Det er ikke lov å gjøre dette.</h1> Tekst utenfor.</li><li>Dette er helt ok</li></ul>
        |<ul><li><h2>Det er ikke lov å gjøre dette.</h2></li><li>Dette er helt ok</li></ul>
        |<ol><li><h3>Det er ikke lov å gjøre dette.</h3></li><li>Dette er helt ok</li></ol>
        |<ol><li><h4>Det er ikke lov å gjøre dette.</h4></li><li>Dette er helt ok</li></ol>
      """.stripMargin,
        "en"
      )),
    Some(
      api.Copyright(Some(api.License("publicdomain", None, None)),
                    Some(""),
                    Seq.empty,
                    Seq.empty,
                    Seq.empty,
                    None,
                    None,
                    None)),
    Some(api.ArticleTag(Seq.empty, "en")),
    Seq.empty,
    None,
    None,
    Some(api.ArticleMetaDescription("so meta", "en")),
    None,
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    "ndalId54321",
    "standard",
    Seq("en"),
    Seq.empty
  )

  val (nodeId, nodeId2) = ("1234", "4321")
  val sampleTitle = ArticleTitle("title", "en")

  val visualElement = VisualElement(
    s"""<$resourceHtmlEmbedTag  data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1" data-size="" />""",
    "nb")

  val sampleConcept = Concept(
    Some(1),
    Seq(ConceptTitle("Tittel for begrep", "nb")),
    Seq(ConceptContent("Innhold for begrep", "nb")),
    Some(Copyright(Some("publicdomain"), Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None)),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate
  )

  val sampleApiConcept = api.Concept(
    1,
    Some(api.ConceptTitle("Tittel for begrep", "nb")),
    Some(api.ConceptContent("Innhold for begrep", "nb")),
    Some(
      api.Copyright(Some(api.License("publicdomain", None, None)),
                    Some(""),
                    Seq.empty,
                    Seq.empty,
                    Seq.empty,
                    None,
                    None,
                    None)),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    Set("nb")
  )

  val sampleApiAgreement = api.Agreement(
    1,
    "title",
    "content",
    api.Copyright(Some(api.License("publicdomain", None, None)), Some(""), Seq(), List(), List(), None, None, None),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    "ndalId54321"
  )

  val sampleDomainAgreement = Agreement(
    id = Some(1),
    title = "Title",
    content = "Content",
    copyright = byNcSaCopyright,
    created = DateTime.now().minusDays(4).toDate,
    updated = DateTime.now().minusDays(2).toDate,
    updatedBy = userWithWriteAccess.id
  )

  val sampleBySaDomainAgreement = Agreement(
    id = Some(1),
    title = "Title",
    content = "Content",
    copyright = Copyright(Some(CC_BY_SA.toString), Some("Origin"), List(), List(), List(), None, None, None),
    created = DateTime.now().minusDays(4).toDate,
    updated = DateTime.now().minusDays(2).toDate,
    updatedBy = userWithWriteAccess.id
  )

  val newAgreement = NewAgreement("newTitle",
                                  "newString",
                                  api.NewAgreementCopyright(Some(api.License("by-sa", None, None)),
                                                            Some(""),
                                                            List(),
                                                            List(),
                                                            List(),
                                                            None,
                                                            None,
                                                            None))
  val statusWithAwaitingPublishing = Set(ArticleStatus.DRAFT, ArticleStatus.QUEUED_FOR_PUBLISHING)
  val statusWithPublished = domain.Status(ArticleStatus.PUBLISHED, Set.empty)
  val statusWithDraft = domain.Status(ArticleStatus.DRAFT, Set.empty)
  val statusWithProposal = domain.Status(ArticleStatus.PROPOSAL, Set.empty)
  val statusWithUserTest = domain.Status(ArticleStatus.USER_TEST, Set.empty)
  val statusWithAwaitingQA = domain.Status(ArticleStatus.AWAITING_QUALITY_ASSURANCE, Set.empty)
  val statusWithQueuedForPublishing = domain.Status(ArticleStatus.QUEUED_FOR_PUBLISHING, Set.empty)

  val sampleLearningPath = LearningPath(Some(1), Seq.empty)
  val sampleLearningStep = LearningStep(Some(1), Seq.empty)
  val sampleEmbedUrl = EmbedUrl("/article/1", "nb", "oembed")
}
