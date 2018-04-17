/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import java.util.concurrent.Executors

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.integration.Elastic4sClient
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.api.{AgreementSearchResult, ResultWindowTooLargeException}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.service.ConverterService
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.searches.queries.BoolQueryDefinition
import org.elasticsearch.ElasticsearchException
import org.elasticsearch.index.IndexNotFoundException

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait AgreementSearchService {
  this: Elastic4sClient
    with SearchConverterService
    with SearchService
    with AgreementIndexService
    with ConverterService =>
  val agreementSearchService: AgreementSearchService

  class AgreementSearchService extends LazyLogging with SearchService[api.AgreementSummary] {
    override val searchIndex: String = DraftApiProperties.AgreementSearchIndex

    override def hitToApiModel(hit: String, language: String): api.AgreementSummary = {
      searchConverterService.hitAsAgreementSummary(hit)
    }

    def all(withIdIn: List[Long],
            license: Option[String],
            page: Int,
            pageSize: Int,
            sort: Sort.Value): Try[AgreementSearchResult] = {
      executeSearch(withIdIn, license, sort, page, pageSize, boolQuery())
    }

    def matchingQuery(query: String,
                      withIdIn: List[Long],
                      license: Option[String],
                      page: Int,
                      pageSize: Int,
                      sort: Sort.Value): Try[AgreementSearchResult] = {

      val fullQuery = boolQuery()
        .must(
          boolQuery()
            .should(
              queryStringQuery(query).field("title").boost(2),
              queryStringQuery(query).field("content").boost(1)
            ))

      executeSearch(withIdIn, license, sort, page, pageSize, fullQuery)
    }

    def executeSearch(withIdIn: List[Long],
                      license: Option[String],
                      sort: Sort.Value,
                      page: Int,
                      pageSize: Int,
                      queryBuilder: BoolQueryDefinition): Try[AgreementSearchResult] = {
      val idFilter = if (withIdIn.isEmpty) None else Some(idsQuery(withIdIn))

      val filters = List(idFilter)
      val filteredSearch = queryBuilder.filter(filters.flatten)

      val (startAt, numResults) = getStartAtAndNumResults(page, pageSize)
      val requestedResultWindow = pageSize * page
      if (requestedResultWindow > DraftApiProperties.ElasticSearchIndexMaxResultWindow) {
        logger.info(
          s"Max supported results are ${DraftApiProperties.ElasticSearchIndexMaxResultWindow}, user requested $requestedResultWindow")
        Failure(new ResultWindowTooLargeException())
      } else {
        e4sClient.execute {
          search(searchIndex)
            .size(numResults)
            .from(startAt)
            .query(filteredSearch)
            .sortBy(getSortDefinition(sort))
        } match {
          case Success(response) =>
            Success(
              AgreementSearchResult(
                response.result.totalHits,
                page,
                numResults,
                Language.NoLanguage,
                getHits(response.result, Language.NoLanguage)
              ))
          case Failure(ex) =>
            errorHandler(ex)
        }
      }
    }

    override def scheduleIndexDocuments() = {
      implicit val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
      val f = Future {
        agreementIndexService.indexDocuments
      }

      f.failed.foreach(t => logger.warn("Unable to create index: " + t.getMessage, t))
      f.foreach {
        case Success(reindexResult) =>
          logger.info(
            s"Completed indexing of ${reindexResult.totalIndexed} agreements in ${reindexResult.millisUsed} ms.")
        case Failure(ex) => logger.warn(ex.getMessage, ex)
      }
    }

  }
}
