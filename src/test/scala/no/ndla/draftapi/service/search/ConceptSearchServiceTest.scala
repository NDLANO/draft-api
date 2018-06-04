/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import no.ndla.draftapi.DraftApiProperties.DefaultPageSize
import no.ndla.draftapi._
import no.ndla.draftapi.integration.Elastic4sClientFactory
import no.ndla.draftapi.model.domain._
import no.ndla.tag.IntegrationTest
import org.joda.time.DateTime

import scala.util.Success

@IntegrationTest
class ConceptSearchServiceTest extends UnitSuite with TestEnvironment {

  val esPort = 9200

  override val e4sClient = Elastic4sClientFactory.getClient(searchServer = s"http://localhost:$esPort")

  override val conceptSearchService = new ConceptSearchService
  override val conceptIndexService = new ConceptIndexService
  override val converterService = new ConverterService
  override val searchConverterService = new SearchConverterService

  val byNcSa = Copyright(Some("by-nc-sa"),
                         Some("Gotham City"),
                         List(Author("Forfatter", "DC Comics")),
                         List(),
                         List(),
                         None,
                         None,
                         None)

  val publicDomain = Copyright(Some("publicdomain"),
                               Some("Metropolis"),
                               List(Author("Forfatter", "Bruce Wayne")),
                               List(),
                               List(),
                               None,
                               None,
                               None)

  val copyrighted = Copyright(Some("copyrighted"),
                              Some("New York"),
                              List(Author("Forfatter", "Clark Kent")),
                              List(),
                              List(),
                              None,
                              None,
                              None)

  val today = DateTime.now()

  val concept1 = TestData.sampleConcept.copy(
    id = Option(1),
    title = List(ConceptTitle("Batmen er på vift med en bil", "nb")),
    content =
      List(ConceptContent("Bilde av en <strong>bil</strong> flaggermusmann som vifter med vingene <em>bil</em>.", "nb"))
  )

  val concept2 = TestData.sampleConcept.copy(
    id = Option(2),
    title = List(ConceptTitle("Pingvinen er ute og går", "nb")),
    content = List(ConceptContent("<p>Bilde av en</p><p> en <em>pingvin</em> som vagger borover en gate</p>", "nb"))
  )

  val concept3 = TestData.sampleConcept.copy(
    id = Option(3),
    title = List(ConceptTitle("Donald Duck kjører bil", "nb")),
    content = List(ConceptContent("<p>Bilde av en en and</p><p> som <strong>kjører</strong> en rød bil.</p>", "nb"))
  )

  val concept4 = TestData.sampleConcept.copy(
    id = Option(4),
    title = List(ConceptTitle("Superman er ute og flyr", "nb")),
    content =
      List(ConceptContent("<p>Bilde av en flygende mann</p><p> som <strong>har</strong> superkrefter.</p>", "nb"))
  )

  val concept5 = TestData.sampleConcept.copy(
    id = Option(5),
    title = List(ConceptTitle("Hulken løfter biler", "nb")),
    content = List(ConceptContent("<p>Bilde av hulk</p><p> som <strong>løfter</strong> en rød bil.</p>", "nb"))
  )

  val concept6 = TestData.sampleConcept.copy(
    id = Option(6),
    title = List(ConceptTitle("Loke og Tor prøver å fange midgaardsormen", "nb")),
    content = List(
      ConceptContent("<p>Bilde av <em>Loke</em> og <em>Tor</em></p><p> som <strong>fisker</strong> fra Naglfar.</p>",
                     "nb"))
  )

  val concept7 = TestData.sampleConcept.copy(
    id = Option(7),
    title = List(ConceptTitle("Yggdrasil livets tre", "nb")),
    content = List(ConceptContent("<p>Bilde av <em>Yggdrasil</em> livets tre med alle dyrene som bor i det.", "nb"))
  )

  val concept8 = TestData.sampleConcept.copy(
    id = Option(8),
    title = List(ConceptTitle("Baldur har mareritt", "nb")),
    content = List(ConceptContent("<p>Bilde av <em>Baldurs</em> mareritt om Ragnarok.", "nb"))
  )

  val concept9 = TestData.sampleConcept.copy(
    id = Option(9),
    title = List(ConceptTitle("Baldur har mareritt om Ragnarok", "nb")),
    content = List(ConceptContent("<p>Bilde av <em>Baldurs</em> som har  mareritt.", "nb"))
  )

  val concept10 = TestData.sampleConcept.copy(
    id = Option(10),
    title = List(ConceptTitle("Unrelated", "en"), ConceptTitle("Urelatert", "nb")),
    content = List(ConceptContent("Pompel", "en"), ConceptContent("Pilt", "nb"))
  )

  val concept11 = TestData.sampleConcept.copy(id = Option(11),
                                              title = List(ConceptTitle("englando", "en")),
                                              content = List(ConceptContent("englandocontent", "en")))

  override def beforeAll = {
    conceptIndexService.createIndexWithName(DraftApiProperties.DraftSearchIndex)

    conceptIndexService.indexDocument(concept1)
    conceptIndexService.indexDocument(concept2)
    conceptIndexService.indexDocument(concept3)
    conceptIndexService.indexDocument(concept4)
    conceptIndexService.indexDocument(concept5)
    conceptIndexService.indexDocument(concept6)
    conceptIndexService.indexDocument(concept7)
    conceptIndexService.indexDocument(concept8)
    conceptIndexService.indexDocument(concept9)
    conceptIndexService.indexDocument(concept10)
    conceptIndexService.indexDocument(concept11)

    blockUntil(() => {
      conceptSearchService.countDocuments == 11
    })
  }

  override def afterAll() = {
    conceptIndexService.deleteIndexWithName(Some(DraftApiProperties.DraftSearchIndex))
  }

  test("That getStartAtAndNumResults returns SEARCH_MAX_PAGE_SIZE for value greater than SEARCH_MAX_PAGE_SIZE") {
    conceptSearchService.getStartAtAndNumResults(0, 1000) should equal((0, DraftApiProperties.MaxPageSize))
  }

  test(
    "That getStartAtAndNumResults returns the correct calculated start at for page and page-size with default page-size") {
    val page = 74
    val expectedStartAt = (page - 1) * DefaultPageSize
    conceptSearchService.getStartAtAndNumResults(page, DefaultPageSize) should equal((expectedStartAt, DefaultPageSize))
  }

  test("That getStartAtAndNumResults returns the correct calculated start at for page and page-size") {
    val page = 123
    val expectedStartAt = (page - 1) * DefaultPageSize
    conceptSearchService.getStartAtAndNumResults(page, DefaultPageSize) should equal((expectedStartAt, DefaultPageSize))
  }

  test("That all returns all documents ordered by id ascending") {
    val Success(results) =
      conceptSearchService.all(List(), Language.DefaultLanguage, 1, 10, Sort.ByIdAsc, fallback = false)
    val hits = results.results
    results.totalCount should be(10)
    hits.head.id should be(1)
    hits(1).id should be(2)
    hits(2).id should be(3)
    hits(3).id should be(4)
    hits(4).id should be(5)
    hits(5).id should be(6)
    hits(6).id should be(7)
    hits(7).id should be(8)
    hits(8).id should be(9)
    hits.last.id should be(10)
  }

  test("That all returns all documents ordered by id descending") {
    val Success(results) =
      conceptSearchService.all(List(), Language.DefaultLanguage, 1, 10, Sort.ByIdDesc, fallback = false)
    val hits = results.results
    results.totalCount should be(10)
    hits.head.id should be(10)
    hits.last.id should be(1)
  }

  test("That all returns all documents ordered by title ascending") {
    val Success(results) =
      conceptSearchService.all(List(), Language.DefaultLanguage, 1, 10, Sort.ByTitleAsc, fallback = false)
    val hits = results.results

    results.totalCount should be(10)
    hits.head.id should be(8)
    hits(1).id should be(9)
    hits(2).id should be(1)
    hits(3).id should be(3)
    hits(4).id should be(5)
    hits(5).id should be(6)
    hits(6).id should be(2)
    hits(7).id should be(4)
    hits.last.id should be(7)
  }

  test("That all returns all documents ordered by title descending") {
    val Success(results) =
      conceptSearchService.all(List(), Language.DefaultLanguage, 1, 10, Sort.ByTitleDesc, fallback = false)
    val hits = results.results
    results.totalCount should be(10)
    hits.head.id should be(7)
    hits(1).id should be(10)
    hits(2).id should be(4)
    hits(3).id should be(2)
    hits(4).id should be(6)
    hits(5).id should be(5)
    hits(6).id should be(3)
    hits(7).id should be(1)
    hits(8).id should be(9)
    hits.last.id should be(8)

  }

  test("That all filtered by id only returns documents with the given ids") {
    val Success(results) =
      conceptSearchService.all(List(1, 3), Language.DefaultLanguage, 1, 10, Sort.ByIdAsc, fallback = false)
    val hits = results.results
    results.totalCount should be(2)
    hits.head.id should be(1)
    hits.last.id should be(3)
  }

  test("That paging returns only hits on current page and not more than page-size") {
    val Success(page1) =
      conceptSearchService.all(List(), Language.DefaultLanguage, 1, 2, Sort.ByTitleAsc, fallback = false)
    val Success(page2) =
      conceptSearchService.all(List(), Language.DefaultLanguage, 2, 2, Sort.ByTitleAsc, fallback = false)

    val hits1 = page1.results
    page1.totalCount should be(10)
    page1.page should be(1)
    hits1.size should be(2)
    hits1.head.id should be(8)
    hits1.last.id should be(9)

    val hits2 = page2.results
    page2.totalCount should be(10)
    page2.page should be(2)
    hits2.size should be(2)
    hits2.head.id should be(1)
    hits2.last.id should be(3)
  }

  test("That search matches title and content ordered by relevance descending") {
    val Success(results) =
      conceptSearchService.matchingQuery("bil", List(), "nb", 1, 10, Sort.ByRelevanceDesc, fallback = false)
    val hits = results.results

    results.totalCount should be(3)
    hits.head.id should be(5)
    hits(1).id should be(1)
    hits.last.id should be(3)
  }

  test("That search matches title") {
    val Success(results) =
      conceptSearchService.matchingQuery("Pingvinen", List(), "nb", 1, 10, Sort.ByTitleAsc, fallback = false)
    val hits = results.results
    results.totalCount should be(1)
    hits.head.id should be(2)
  }

  test("Searching with logical AND only returns results with all terms") {
    val Success(search1) =
      conceptSearchService.matchingQuery("bilde + bil", List(), "nb", 1, 10, Sort.ByTitleAsc, fallback = false)
    val hits1 = search1.results
    hits1.map(_.id) should equal(Seq(1, 3, 5))

    val Success(search2) =
      conceptSearchService.matchingQuery("batmen + bil", List(), "nb", 1, 10, Sort.ByTitleAsc, fallback = false)
    val hits2 = search2.results
    hits2.map(_.id) should equal(Seq(1))

    val Success(search3) = conceptSearchService.matchingQuery("bil + bilde + -flaggermusmann",
                                                              List(),
                                                              "nb",
                                                              1,
                                                              10,
                                                              Sort.ByTitleAsc,
                                                              fallback = false)
    val hits3 = search3.results
    hits3.map(_.id) should equal(Seq(3, 5))

    val Success(search4) =
      conceptSearchService.matchingQuery("bil + -hulken", List(), "nb", 1, 10, Sort.ByTitleAsc, fallback = false)
    val hits4 = search4.results
    hits4.map(_.id) should equal(Seq(1, 3))
  }

  test("search in content should be ranked lower than title") {
    val Success(search) = conceptSearchService.matchingQuery("mareritt + ragnarok",
                                                             List(),
                                                             "nb",
                                                             1,
                                                             10,
                                                             Sort.ByRelevanceDesc,
                                                             fallback = false)
    val hits = search.results
    hits.map(_.id) should equal(Seq(9, 8))
  }

  test("Search should return language it is matched in") {
    val Success(searchEn) =
      conceptSearchService.matchingQuery("Unrelated", List(), "all", 1, 10, Sort.ByIdAsc, fallback = false)
    val Success(searchNb) =
      conceptSearchService.matchingQuery("Urelatert", List(), "all", 1, 10, Sort.ByIdAsc, fallback = false)

    searchEn.totalCount should be(1)
    searchEn.results.head.title.language should be("en")
    searchEn.results.head.title.title should be("Unrelated")
    searchEn.results.head.content.language should be("en")
    searchEn.results.head.content.content should be("Pompel")

    searchNb.totalCount should be(1)
    searchNb.results.head.title.language should be("nb")
    searchNb.results.head.title.title should be("Urelatert")
    searchNb.results.head.content.language should be("nb")
    searchNb.results.head.content.content should be("Pilt")
  }

  test("Search for all languages should return all concepts in correct language") {
    val Success(search) =
      conceptSearchService.all(List(), Language.AllLanguages, 1, 100, Sort.ByIdAsc, fallback = false)
    val hits = search.results

    search.totalCount should equal(11)
    hits(0).id should be(1)
    hits(0).title.language should be("nb")
    hits(1).id should be(2)
    hits(2).id should be(3)
    hits(3).id should be(4)
    hits(4).id should be(5)
    hits(5).id should be(6)
    hits(6).id should be(7)
    hits(7).id should be(8)
    hits(8).id should be(9)
    hits(9).id should be(10)
    hits(9).title.language should be("nb")
    hits(10).id should be(11)
    hits(10).title.language should be("en")
  }

  test("That searching with fallback parameter returns concept in language priority even if doesnt match on language") {
    val Success(search) = conceptSearchService.all(List(9, 10, 11), "en", 1, 10, Sort.ByIdAsc, fallback = true)

    search.totalCount should equal(3)
    search.results.head.id should equal(9)
    search.results.head.title.language should equal("nb")
    search.results(1).id should equal(10)
    search.results(1).title.language should equal("en")
    search.results(2).id should equal(11)
    search.results(2).title.language should equal("en")
  }

  def blockUntil(predicate: () => Boolean) = {
    var backoff = 0
    var done = false

    while (backoff <= 16 && !done) {
      if (backoff > 0) Thread.sleep(200 * backoff)
      backoff = backoff + 1
      try {
        done = predicate()
      } catch {
        case e: Throwable => println("problem while testing predicate", e)
      }
    }

    require(done, s"Failed waiting for predicate")
  }
}
