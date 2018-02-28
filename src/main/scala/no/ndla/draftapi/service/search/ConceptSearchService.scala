/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import java.util.concurrent.Executors

import com.sksamuel.elastic4s.searches.queries.BoolQueryDefinition
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.integration.Elastic4sClient
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.service.ConverterService
import org.elasticsearch.ElasticsearchException
import org.elasticsearch.index.IndexNotFoundException
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.searches.ScoreMode
import no.ndla.draftapi.model.api.ResultWindowTooLargeException
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.json4s._
import org.json4s.native.JsonMethods._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait ConceptSearchService {
  this: Elastic4sClient with SearchService with ConceptIndexService with ConverterService with SearchConverterService =>
  val conceptSearchService: ConceptSearchService

  class ConceptSearchService extends LazyLogging with SearchService[api.ConceptSummary] {
    override val searchIndex: String = DraftApiProperties.ConceptSearchIndex

    override def hitToApiModel(hitString: String, language: String): api.ConceptSummary =
      searchConverterService.hitAsConceptSummary(hitString, language)

    def all(withIdIn: List[Long], language: String, page: Int, pageSize: Int, sort: Sort.Value): api.ConceptSearchResult = {
      executeSearch(withIdIn, language, sort, page, pageSize, boolQuery())
    }

    def matchingQuery(query: String, withIdIn: List[Long], searchLanguage: String, page: Int, pageSize: Int, sort: Sort.Value): api.ConceptSearchResult = {
      val language = if (searchLanguage == Language.AllLanguages) "*" else searchLanguage

      val titleSearch = simpleStringQuery(query).field(s"title.$language", 2)
      val contentSearch = simpleStringQuery(query).field(s"content.$language", 1)

      val fullQuery = boolQuery()
        .must(boolQuery()
          .should(
            titleSearch,
            contentSearch
          )
        )

      executeSearch(withIdIn, language, sort, page, pageSize, fullQuery)
    }

    def executeSearch(withIdIn: List[Long], language: String, sort: Sort.Value, page: Int, pageSize: Int, queryBuilder: BoolQueryDefinition): api.ConceptSearchResult = {
      val idFilter = if (withIdIn.isEmpty) None else Some(idsQuery(withIdIn))

      val (languageFilter, searchLanguage) = language match {
        case "" | Language.AllLanguages | "*" =>
          (None, "*")
        case lang =>
          (Some(existsQuery(s"title.$lang")), lang)
      }

      val filters = List(idFilter, languageFilter)
      val filteredSearch = queryBuilder.filter(filters.flatten)

      val (startAt, numResults) = getStartAtAndNumResults(page, pageSize)
      val requestedResultWindow = pageSize * page
      if (requestedResultWindow > DraftApiProperties.ElasticSearchIndexMaxResultWindow) {
        logger.info(s"Max supported results are ${DraftApiProperties.ElasticSearchIndexMaxResultWindow}, user requested $requestedResultWindow")
        throw new ResultWindowTooLargeException()
      }

      val searchExec =
        search(searchIndex)
          .size(numResults)
          .from(startAt)
          .query(filteredSearch)
          .highlighting(highlight("*"))
          .sortBy(getSortDefinition(sort, searchLanguage))

      e4sClient.execute(searchExec) match {
        case Success(response) =>
          api.ConceptSearchResult(
            response.result.totalHits,
            page,
            numResults,
            if (searchLanguage == "*") Language.AllLanguages else searchLanguage,
            getHits(response.result, language))
        case Failure(ex) =>
          errorHandler(Failure(ex))
      }

    }

    protected def errorHandler[T](failure: Failure[T]) = {
      failure match {
        case Failure(e: NdlaSearchException) =>
          e.rf.status match {
            case notFound: Int if notFound == 404 =>
              logger.error(s"Index $searchIndex not found. Scheduling a reindex.")
              scheduleIndexDocuments()
              throw new IndexNotFoundException(s"Index $searchIndex not found. Scheduling a reindex")
            case _ =>
              logger.error(e.getMessage)
              throw new ElasticsearchException(s"Unable to execute search in $searchIndex", e.getMessage)
          }
        case Failure(t: Throwable) => throw t
      }
    }

    private def scheduleIndexDocuments() = {
      implicit val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
      val f = Future {
        conceptIndexService.indexDocuments
      }

      f.failed.foreach(t => logger.warn("Unable to create index: " + t.getMessage, t))
      f.foreach {
        case Success(reindexResult) => logger.info(s"Completed indexing of ${reindexResult.totalIndexed} concepts in ${reindexResult.millisUsed} ms.")
        case Failure(ex) => logger.warn(ex.getMessage, ex)
      }
    }

  }
}
