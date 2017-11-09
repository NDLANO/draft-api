/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service.search

import no.ndla.draftapi.DraftApiProperties.DefaultPageSize
import no.ndla.draftapi._
import no.ndla.draftapi.integration.JestClientFactory
import no.ndla.draftapi.model.domain._
import no.ndla.tag.IntegrationTest
import org.joda.time.DateTime

@IntegrationTest
class AgreementSearchServiceTest extends UnitSuite with TestEnvironment {

  val esPort = 9200

  override val jestClient = JestClientFactory.getClient(searchServer = s"http://localhost:$esPort")

  override val agreementSearchService = new AgreementSearchService
  override val agreementIndexService = new AgreementIndexService
  override val converterService = new ConverterService
  override val searchConverterService = new SearchConverterService

  val byNcSa = Copyright("by-nc-sa", "Gotham City", List(Author("Forfatter", "DC Comics")), List(), List(), None, None, None)
  val publicDomain = Copyright("publicdomain", "Metropolis", List(Author("Forfatter", "Bruce Wayne")), List(), List(), None, None, None)
  val copyrighted = Copyright("copyrighted", "New York", List(Author("Forfatter", "Clark Kent")), List(), List(), None, None, None)

  val today = DateTime.now()


  val sampleAgreement = new Agreement(
    Some(1),
    "title",
    "content",
    AgreementContact("Jim Jum", "j@j.com"),
    AgreementContact("Bim Bum", "b@b.com"),
    byNcSa,
    today.minusDays(2).toDate,
    today.minusDays(4).toDate,
    "ndla1234"
  )

  val agreement1 = sampleAgreement.copy(id=Some(2), title="Aper får lov", content = "Aper får kjempe seg fremover")
  val agreement2 = sampleAgreement.copy(id=Some(3), title="Ugler er slemme", content = "Ugler er de slemmeste dyrene")
  val agreement3 = sampleAgreement.copy(id=Some(4), title="Tyven stjeler penger", content = "Det er ikke hemmelig at tyven er den som stjeler penger")
  val agreement4 = sampleAgreement.copy(id=Some(5), title="Vi får låne bildene", content = "Vi får låne bildene av kjeltringene")
  val agreement5 = sampleAgreement.copy(id=Some(6), title="Kjeltringer er ikke velkomne", content = "De er slemmere enn kjeft")
  val agreement6 = sampleAgreement.copy(id=Some(7), title="Du er en tyv", content = "Det er du som er tyven")
  val agreement7 = sampleAgreement.copy(id=Some(8), title="Lurerier er ikke lov", content = "Lurerier er bare lov dersom du er en tyv")
  val agreement8 = sampleAgreement.copy(id=Some(9), title="Hvorfor er aper så slemme", content = "Har du blitt helt ape")
  val agreement9 = sampleAgreement.copy(id=Some(10), title="Du er en av dem du", content = "Det er ikke snilt å være en av dem")


  override def beforeAll = {
    agreementIndexService.createIndexWithName(DraftApiProperties.DraftSearchIndex)

    agreementIndexService.indexDocument(agreement1)
    agreementIndexService.indexDocument(agreement2)
    agreementIndexService.indexDocument(agreement3)
    agreementIndexService.indexDocument(agreement4)
    agreementIndexService.indexDocument(agreement5)
    agreementIndexService.indexDocument(agreement6)
    agreementIndexService.indexDocument(agreement7)
    agreementIndexService.indexDocument(agreement8)
    agreementIndexService.indexDocument(agreement9)

    blockUntil(() => {
      println(s"num docs: ${agreementSearchService.countDocuments}")
      agreementSearchService.countDocuments == 9
    })
  }

  override def afterAll() = {
    agreementIndexService.deleteIndex(Some(DraftApiProperties.DraftSearchIndex))
  }

  test("That getStartAtAndNumResults returns SEARCH_MAX_PAGE_SIZE for value greater than SEARCH_MAX_PAGE_SIZE") {
    agreementSearchService.getStartAtAndNumResults(0, 1000) should equal((0, DraftApiProperties.MaxPageSize))
  }

  test("That getStartAtAndNumResults returns the correct calculated start at for page and page-size with default page-size") {
    val page = 74
    val expectedStartAt = (page - 1) * DefaultPageSize
    agreementSearchService.getStartAtAndNumResults(page, DefaultPageSize) should equal((expectedStartAt, DefaultPageSize))
  }

  test("That getStartAtAndNumResults returns the correct calculated start at for page and page-size") {
    val page = 123
    val expectedStartAt = (page - 1) * DefaultPageSize
    agreementSearchService.getStartAtAndNumResults(page, DefaultPageSize) should equal((expectedStartAt, DefaultPageSize))
  }

  test("That all returns all documents ordered by id ascending") {
    val results = agreementSearchService.all(List(), None, 1, 10, Sort.ByIdAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(8)
    hits.head.id should be(1)
    hits(1).id should be(2)
    hits(2).id should be(3)
    hits(3).id should be(5)
    hits(4).id should be(6)
    hits(5).id should be(7)
    hits(6).id should be(8)
    hits.last.id should be(9)
  }

  test("That all returns all documents ordered by id descending") {
    val results = agreementSearchService.all(List(), None, 1, 10, Sort.ByIdDesc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(8)
    hits.head.id should be (9)
    hits.last.id should be (1)
  }

  test("That all returns all documents ordered by title ascending") {
    val results = agreementSearchService.all(List(), None, 1, 10, Sort.ByTitleAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(8)
    hits.head.id should be(8)
    hits(1).id should be(9)
    hits(2).id should be(1)
    hits(3).id should be(3)
    hits(4).id should be(5)
    hits(5).id should be(6)
    hits(6).id should be(2)
    hits.last.id should be(7)
  }

  test("That all returns all documents ordered by title descending") {
    val results = agreementSearchService.all(List(), None, 1, 10, Sort.ByTitleDesc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(8)
    hits.head.id should be(7)
    hits(1).id should be(2)
    hits(2).id should be(6)
    hits(3).id should be(5)
    hits(4).id should be(3)
    hits(5).id should be(1)
    hits.last.id should be(8)
  }

  test("That all returns all documents ordered by lastUpdated descending") {
    val results = agreementSearchService.all(List(), None, 1, 10, Sort.ByLastUpdatedDesc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(8)
    hits.head.id should be(3)
    hits.last.id should be(5)
  }

  test("That all returns all documents ordered by lastUpdated ascending") {
    val results = agreementSearchService.all(List(), None, 1, 10, Sort.ByLastUpdatedAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(8)
    hits.head.id should be(5)
    hits(1).id should be(6)
    hits(2).id should be(7)
    hits(3).id should be(8)
    hits(4).id should be(9)
    hits(5).id should be(1)
    hits(6).id should be(2)
    hits.last.id should be(3)
  }

  test("That all filtering on license only returns documents with given license") {
    val results = agreementSearchService.all(List(), Some("publicdomain"), 1, 10, Sort.ByTitleAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(7)
    hits.head.id should be(8)
    hits(1).id should be(9)
    hits(2).id should be(3)
    hits(3).id should be(5)
    hits(4).id should be(6)
    hits(5).id should be(2)
    hits.last.id should be(7)
  }

  test("That all filtered by id only returns documents with the given ids") {
    val results = agreementSearchService.all(List(1, 3), None, 1, 10, Sort.ByIdAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(2)
    hits.head.id should be(1)
    hits.last.id should be(3)
  }

  test("That paging returns only hits on current page and not more than page-size") {
    val page1 = agreementSearchService.all(List(), None, 1, 2, Sort.ByTitleAsc)
    val hits1 = converterService.getAgreementHits(page1.response)
    page1.totalCount should be(8)
    page1.page should be(1)
    hits1.size should be(2)
    hits1.head.id should be(8)
    hits1.last.id should be(9)

    val page2 = agreementSearchService.all(List(), None, 2, 2, Sort.ByTitleAsc)
    val hits2 = converterService.getAgreementHits(page2.response)
    page2.totalCount should be(8)
    page2.page should be(2)
    hits2.size should be(2)
    hits2.head.id should be(1)
    hits2.last.id should be(3)
  }

  test("mathcingQuery should filter results based on an article type filter") {
    val results = agreementSearchService.matchingQuery("bil", List(), None, 1, 10, Sort.ByRelevanceDesc)
    results.totalCount should be(0)

    val results2 = agreementSearchService.matchingQuery("bil", List(), None, 1, 10, Sort.ByRelevanceDesc)
    results2.totalCount should be(3)
  }

  test("That search matches title and html-content ordered by relevance descending") {
    val results = agreementSearchService.matchingQuery("bil", List(), None, 1, 10, Sort.ByRelevanceDesc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(3)
    hits.head.id should be(5)
    hits(1).id should be(1)
    hits.last.id should be(3)
  }

  test("That search combined with filter by id only returns documents matching the query with one of the given ids") {
    val results = agreementSearchService.matchingQuery("bil", List(3), None, 1, 10, Sort.ByRelevanceDesc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(1)
    hits.head.id should be(3)
  }

  test("That search matches title") {
    val results = agreementSearchService.matchingQuery("Pingvinen", List(), None, 1, 10, Sort.ByTitleAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(1)
    hits.head.id should be(2)
  }

  test("That search matches tags") {
    val results = agreementSearchService.matchingQuery("and", List(), None, 1, 10, Sort.ByTitleAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(1)
    hits.head.id should be(3)
  }

  test("That search does not return superman since it has license copyrighted and license is not specified") {
    val results = agreementSearchService.matchingQuery("supermann", List(), None, 1, 10, Sort.ByTitleAsc)
    results.totalCount should be(0)
  }

  test("That search returns superman since license is specified as copyrighted") {
    val results = agreementSearchService.matchingQuery("supermann", List(), Some("copyrighted"), 1, 10, Sort.ByTitleAsc)
    val hits = converterService.getAgreementHits(results.response)
    results.totalCount should be(1)
    hits.head.id should be(4)
  }

  test("Searching with logical AND only returns results with all terms") {
    val search1 = agreementSearchService.matchingQuery("bilde + bil", List(), None, 1, 10, Sort.ByTitleAsc)
    val hits1 = converterService.getAgreementHits(search1.response)
    hits1.map(_.id) should equal (Seq(1, 3, 5))

    val search2 = agreementSearchService.matchingQuery("batmen + bil", List(), None, 1, 10, Sort.ByTitleAsc)
    val hits2 = converterService.getAgreementHits(search2.response)
    hits2.map(_.id) should equal (Seq(1))

    val search3 = agreementSearchService.matchingQuery("bil + bilde - flaggermusmann", List(), None, 1, 10, Sort.ByTitleAsc)
    val hits3 = converterService.getAgreementHits(search3.response)
    hits3.map(_.id) should equal (Seq(1, 3, 5))

    val search4 = agreementSearchService.matchingQuery("bil - hulken", List(), None, 1, 10, Sort.ByTitleAsc)
    val hits4 = converterService.getAgreementHits(search4.response)
    hits4.map(_.id) should equal (Seq(1, 3, 5))
  }

  test("search in content should be ranked lower than introduction and title") {
    val search = agreementSearchService.matchingQuery("mareritt + ragnarok", List(), None, 1, 10, Sort.ByRelevanceDesc)
    val hits = converterService.getAgreementHits(search.response)
    hits.map(_.id) should equal (Seq(9, 8))
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
