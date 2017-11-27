/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service.search

import com.google.gson.JsonObject
import com.typesafe.scalalogging.LazyLogging
import io.searchbox.core.Search
import io.searchbox.params.Parameters
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.integration.ElasticClient
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.api.ResultWindowTooLargeException
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.service.ConverterService
import org.apache.lucene.search.join.ScoreMode
import org.elasticsearch.ElasticsearchException
import org.elasticsearch.index.IndexNotFoundException
import org.elasticsearch.index.query._
import org.elasticsearch.search.builder.SearchSourceBuilder

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait AgreementSearchService {
  this: ElasticClient with SearchConverterService with SearchService with AgreementIndexService with ConverterService =>
  val agreementSearchService: AgreementSearchService

  class AgreementSearchService extends LazyLogging with SearchService[api.AgreementSummary] {
    private val noCopyright = QueryBuilders.boolQuery().mustNot(QueryBuilders.termQuery("license", "copyrighted"))

    override val searchIndex: String = DraftApiProperties.AgreementSearchIndex

    override def hitToApiModel(hit: JsonObject, language: String): api.AgreementSummary = {
      searchConverterService.hitAsAgreementSummary(hit)
    }

    def all(withIdIn: List[Long], license: Option[String], page: Int, pageSize: Int, sort: Sort.Value): SearchResult = {
      val fullSearch = QueryBuilders.boolQuery()
      executeSearch(withIdIn, license, sort, page, pageSize, fullSearch)
    }

    def matchingQuery(query: String, withIdIn: List[Long], license: Option[String], page: Int, pageSize: Int, sort: Sort.Value): SearchResult = {
      val fullQuery = QueryBuilders.boolQuery()
        .must(QueryBuilders.boolQuery()
            .should(QueryBuilders.queryStringQuery(query).field("title")).boost(2)
            .should(QueryBuilders.queryStringQuery(query).field("content")).boost(1)
        )

      executeSearch(withIdIn, license, sort, page, pageSize, fullQuery)
    }

    def executeSearch(withIdIn: List[Long], license: Option[String], sort: Sort.Value, page: Int, pageSize: Int, queryBuilder: BoolQueryBuilder): SearchResult = {

      val filteredSearch = license match {
        case None => queryBuilder.filter(noCopyright)
        case Some(lic) => queryBuilder.filter(QueryBuilders.termQuery("license", lic))
      }

      val idFilteredSearch = withIdIn match {
        case head :: tail => filteredSearch.filter(QueryBuilders.idsQuery(DraftApiProperties.AgreementSearchDocument).addIds(head.toString :: tail.map(_.toString): _*))
        case Nil => filteredSearch
      }

      val searchQuery = new SearchSourceBuilder().query(idFilteredSearch).sort(getSortDefinition(sort))

      val (startAt, numResults) = getStartAtAndNumResults(page, pageSize)
      val request = new Search.Builder(searchQuery.toString)
        .addIndex(searchIndex)
        .setParameter(Parameters.SIZE, numResults)
        .setParameter("from", startAt)

      val requestedResultWindow = pageSize * page
      if (requestedResultWindow > DraftApiProperties.ElasticSearchIndexMaxResultWindow) {
        logger.info(s"Max supported results are ${DraftApiProperties.ElasticSearchIndexMaxResultWindow}, user requested ${requestedResultWindow}")
        throw new ResultWindowTooLargeException()
      }

      jestClient.execute(request.build()) match {
        case Success(response) => SearchResult(response.getTotal.toLong, page, numResults, Language.NoLanguage, response)
        case Failure(f) => errorHandler(Failure(f))
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
