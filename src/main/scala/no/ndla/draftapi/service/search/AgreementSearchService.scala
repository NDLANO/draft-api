/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service.search

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.integration.{Elastic4sClient, ElasticClient}
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.api.{AgreementSearchResult, ResultWindowTooLargeException}
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.service.ConverterService
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.searches.queries.BoolQueryDefinition
import org.elasticsearch.ElasticsearchException
import org.elasticsearch.index.IndexNotFoundException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait AgreementSearchService {
  this: Elastic4sClient with ElasticClient with SearchConverterService with SearchService with AgreementIndexService with ConverterService =>
  val agreementSearchService: AgreementSearchService

  class AgreementSearchService extends LazyLogging with SearchService[api.AgreementSummary] {
    private val noCopyright = boolQuery().not(termQuery("license", "copyrighted"))

    override val searchIndex: String = DraftApiProperties.AgreementSearchIndex

    override def hitToApiModel(hit: String, language: String): api.AgreementSummary = {
      searchConverterService.hitAsAgreementSummary(hit)
    }

    def all(withIdIn: List[Long], license: Option[String], page: Int, pageSize: Int, sort: Sort.Value): AgreementSearchResult = {
      val fullSearch = boolQuery()
      executeSearch(withIdIn, license, sort, page, pageSize, fullSearch)
    }

    def matchingQuery(query: String, withIdIn: List[Long], license: Option[String], page: Int, pageSize: Int, sort: Sort.Value): AgreementSearchResult = {
      val fullQuery = boolQuery()
        .must(boolQuery()
            .should(queryStringQuery(query).field("title")).boost(2)
            .should(queryStringQuery(query).field("content")).boost(1)
        )

      executeSearch(withIdIn, license, sort, page, pageSize, fullQuery)
    }

    def executeSearch(withIdIn: List[Long], license: Option[String], sort: Sort.Value, page: Int, pageSize: Int, queryBuilder: BoolQueryDefinition): AgreementSearchResult = {

      val licenseFilter = license match {
        case None => Some(noCopyright)
        case Some(lic) => Some(termQuery("license", lic))
      }

      val idFilter = if (withIdIn.isEmpty) None else Some(idsQuery(withIdIn))

      val filters = List(licenseFilter, idFilter)
      val filteredSearch = queryBuilder.filter(filters.flatten)

      val (startAt, numResults) = getStartAtAndNumResults(page, pageSize)
      val requestedResultWindow = pageSize * page
      if (requestedResultWindow > DraftApiProperties.ElasticSearchIndexMaxResultWindow) {
        logger.info(s"Max supported results are ${DraftApiProperties.ElasticSearchIndexMaxResultWindow}, user requested $requestedResultWindow")
        throw new ResultWindowTooLargeException()
      }

      e4sClient.execute{
        search(searchIndex)
          .size(numResults)
          .from(startAt)
          .query(filteredSearch)
          .sortBy(getSortDefinition(sort))
      } match {
        case Success(response) =>
          AgreementSearchResult(response.result.totalHits, page, numResults, Language.NoLanguage, getHits(response.result, Language.NoLanguage, hitToApiModel))

        case Failure(ex) =>
          errorHandler(Failure(ex))
      }

    }

    protected def errorHandler[T](failure: Failure[T]) = {
      failure match {
        case Failure(e: NdlaSearchException) =>
          e.getResponse.getResponseCode match {
            case notFound: Int if notFound == 404 =>
              logger.error(s"Index $searchIndex not found. Scheduling a reindex.")
              scheduleIndexDocuments()
              throw new IndexNotFoundException(s"Index $searchIndex not found. Scheduling a reindex")
            case _ =>
              logger.error(e.getResponse.getErrorMessage)
              throw new ElasticsearchException(s"Unable to execute search in $searchIndex", e.getResponse.getErrorMessage)
          }
        case Failure(t: Throwable) => throw t
      }
    }

    private def scheduleIndexDocuments() = {
      val f = Future {
        agreementIndexService.indexDocuments
      }

      f.failed.foreach(t => logger.warn("Unable to create index: " + t.getMessage, t))
      f.foreach {
        case Success(reindexResult) => logger.info(s"Completed indexing of ${reindexResult.totalIndexed} documents in ${reindexResult.millisUsed} ms.")
        case Failure(ex) => logger.warn(ex.getMessage, ex)
      }
    }

  }
}
