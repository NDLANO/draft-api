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
class AgreementSearchServiceTest extends UnitSuite with TestEnvironment {

  val esPort = 9200

  override val e4sClient = Elastic4sClientFactory.getClient(searchServer = s"http://localhost:$esPort")

  override val agreementSearchService = new AgreementSearchService
  override val agreementIndexService = new AgreementIndexService
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

  val sampleAgreement = new Agreement(
    Some(1),
    "title",
    "content",
    byNcSa,
    today.minusDays(2).toDate,
    today.minusDays(4).toDate,
    "ndla1234"
  )

  val agreement1 = sampleAgreement.copy(id = Some(2), title = "Aper får lov", content = "Aper får kjempe seg fremover")

  val agreement2 =
    sampleAgreement.copy(id = Some(3), title = "Ugler er slemme", content = "Ugler er de slemmeste dyrene")

  val agreement3 = sampleAgreement.copy(id = Some(4),
                                        title = "Tyven stjeler penger",
                                        content = "Det er ikke hemmelig at tyven er den som stjeler penger")

  val agreement4 =
    sampleAgreement.copy(id = Some(5), title = "Vi får låne bildene", content = "Vi får låne bildene av kjeltringene")

  val agreement5 =
    sampleAgreement.copy(id = Some(6), title = "Kjeltringer er ikke velkomne", content = "De er slemmere enn kjeft")
  val agreement6 = sampleAgreement.copy(id = Some(7), title = "Du er en tyv", content = "Det er du som er tyven")

  val agreement7 = sampleAgreement.copy(id = Some(8),
                                        title = "Lurerier er ikke bra",
                                        content = "Lurerier er bare lov dersom du er en tyv")

  val agreement8 =
    sampleAgreement.copy(id = Some(9), title = "Hvorfor er aper så slemme", content = "Har du blitt helt ape")

  val agreement9 =
    sampleAgreement.copy(id = Some(10), title = "Du er en av dem du", content = "Det er ikke snilt å være en av dem")
  val agreement10 = sampleAgreement.copy(id = Some(11), title = "Woopie", content = "This agreement is not copyrighted")

  override def beforeAll = {
    agreementIndexService.createIndexWithName(DraftApiProperties.AgreementSearchIndex)

    agreementIndexService.indexDocument(agreement1)
    agreementIndexService.indexDocument(agreement2)
    agreementIndexService.indexDocument(agreement3)
    agreementIndexService.indexDocument(agreement4)
    agreementIndexService.indexDocument(agreement5)
    agreementIndexService.indexDocument(agreement6)
    agreementIndexService.indexDocument(agreement7)
    agreementIndexService.indexDocument(agreement8)
    agreementIndexService.indexDocument(agreement9)
    agreementIndexService.indexDocument(agreement10)

    blockUntil(() => {
      agreementSearchService.countDocuments == 10
    })
  }

  override def afterAll() = {
    agreementIndexService.deleteIndexWithName(Some(DraftApiProperties.AgreementSearchIndex))
  }

  test("That getStartAtAndNumResults returns SEARCH_MAX_PAGE_SIZE for value greater than SEARCH_MAX_PAGE_SIZE") {
    agreementSearchService.getStartAtAndNumResults(0, 1000) should equal((0, DraftApiProperties.MaxPageSize))
  }

  test(
    "That getStartAtAndNumResults returns the correct calculated start at for page and page-size with default page-size") {
    val page = 74
    val expectedStartAt = (page - 1) * DefaultPageSize
    agreementSearchService.getStartAtAndNumResults(page, DefaultPageSize) should equal(
      (expectedStartAt, DefaultPageSize))
  }

  test("That getStartAtAndNumResults returns the correct calculated start at for page and page-size") {
    val page = 123
    val expectedStartAt = (page - 1) * DefaultPageSize
    agreementSearchService.getStartAtAndNumResults(page, DefaultPageSize) should equal(
      (expectedStartAt, DefaultPageSize))
  }

  test("That all returns all documents ordered by id ascending") {
    val Success(results) = agreementSearchService.all(List(), None, 1, 10, Sort.ByIdAsc)
    val hits = results.results
    results.totalCount should be(10)
    hits(0).id should be(2)
    hits(1).id should be(3)
    hits(2).id should be(4)
    hits(3).id should be(5)
    hits(4).id should be(6)
    hits(5).id should be(7)
    hits(6).id should be(8)
    hits(7).id should be(9)
    hits(8).id should be(10)
    hits(9).id should be(11)
  }

  test("That all returns all documents ordered by id descending") {
    val Success(results) = agreementSearchService.all(List(), None, 1, 10, Sort.ByIdDesc)
    val hits = results.results
    results.totalCount should be(10)
    hits.head.id should be(11)
    hits.last.id should be(2)
  }

  test("That all returns all documents ordered by title ascending") {
    val Success(results) = agreementSearchService.all(List(), None, 1, 10, Sort.ByTitleAsc)
    val hits = results.results
    results.totalCount should be(10)
    hits(0).id should be(2)
    hits(1).id should be(10)
    hits(2).id should be(7)
    hits(3).id should be(9)
    hits(4).id should be(6)
    hits(5).id should be(8)
    hits(6).id should be(4)
    hits(7).id should be(3)
    hits(8).id should be(5)
  }

  test("That all returns all documents ordered by title descending") {
    val Success(results) = agreementSearchService.all(List(), None, 1, 10, Sort.ByTitleDesc)
    val hits = results.results
    results.totalCount should be(10)
    hits(0).id should be(11)
    hits(1).id should be(5)
    hits(2).id should be(3)
    hits(3).id should be(4)
    hits(4).id should be(8)
    hits(5).id should be(6)
    hits(6).id should be(9)
    hits(7).id should be(7)
    hits(8).id should be(10)
    hits(9).id should be(2)
  }

  test("That paging returns only hits on current page and not more than page-size") {
    val Success(page1) = agreementSearchService.all(List(), None, 1, 2, Sort.ByTitleAsc)
    val hits1 = page1.results
    page1.totalCount should be(10)
    page1.page should be(1)
    hits1.size should be(2)
    hits1.head.id should be(2)
    hits1.last.id should be(10)

    val Success(page2) = agreementSearchService.all(List(), None, 2, 2, Sort.ByTitleAsc)
    val hits2 = page2.results
    page2.totalCount should be(10)
    page2.page should be(2)
    hits2.size should be(2)
    hits2.head.id should be(7)
    hits2.last.id should be(9)
  }

  test("That search combined with filter by id only returns documents matching the query with one of the given ids") {
    val Success(results) = agreementSearchService.matchingQuery("Du", List(10), None, 1, 10, Sort.ByRelevanceDesc)
    val hits = results.results
    results.totalCount should be(1)
    hits.head.id should be(10)
  }

  test("That search matches title") {
    val Success(results) = agreementSearchService.matchingQuery("Ugler", List(), None, 1, 10, Sort.ByTitleAsc)
    val hits = results.results
    results.totalCount should be(1)
    hits.head.id should be(3)
  }

  test("That search does not return superman since it has license copyrighted and license is not specified") {
    val Success(results) = agreementSearchService.matchingQuery("supermann", List(), None, 1, 10, Sort.ByTitleAsc)
    results.totalCount should be(0)
  }

  test("Searching with logical AND only returns results with all terms") {
    val Success(search1) = agreementSearchService.matchingQuery("aper + du", List(), None, 1, 10, Sort.ByIdAsc)
    val hits1 = search1.results
    hits1.map(_.id) should equal(Seq(2, 7, 8, 9, 10))

    val Success(search2) = agreementSearchService.matchingQuery("lurerier + dersom", List(), None, 1, 10, Sort.ByIdAsc)
    val hits2 = search2.results
    hits2.map(_.id) should equal(Seq(8))

    val Success(search3) =
      agreementSearchService.matchingQuery("tyv + stjeler - Lurerier", List(), None, 1, 10, Sort.ByIdAsc)
    val hits3 = search3.results
    hits3.map(_.id) should equal(Seq(4, 7, 8))

    val Success(search4) = agreementSearchService.matchingQuery("aper -slemme", List(), None, 1, 10, Sort.ByIdAsc)
    val hits4 = search4.results
    hits4.map(_.id) should equal(Seq(2))
  }

  test("search in content should be ranked lower than title") {
    val Success(search) = agreementSearchService.matchingQuery("lov", List(), None, 1, 10, Sort.ByRelevanceDesc)
    val hits = search.results
    hits.map(_.id) should equal(Seq(8, 2))
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
