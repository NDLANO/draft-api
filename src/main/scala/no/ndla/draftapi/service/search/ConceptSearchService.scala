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
import com.sksamuel.elastic4s.http.ElasticDsl._
import no.ndla.draftapi.model.api.ResultWindowTooLargeException
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait ConceptSearchService {
  this: Elastic4sClient with SearchService with ConceptIndexService with ConverterService with SearchConverterService =>
  val conceptSearchService: ConceptSearchService

  class ConceptSearchService extends LazyLogging with SearchService[api.ConceptSummary] {
    override val searchIndex: String = DraftApiProperties.ConceptSearchIndex

    override def hitToApiModel(hitString: String, language: String): api.ConceptSummary =
      searchConverterService.hitAsConceptSummary(hitString, language)

    def all(withIdIn: List[Long],
            language: String,
            page: Int,
            pageSize: Int,
            sort: Sort.Value,
            fallback: Boolean): Try[api.ConceptSearchResult] =
      executeSearch(withIdIn, language, sort, page, pageSize, boolQuery(), fallback)

    def matchingQuery(query: String,
                      withIdIn: List[Long],
                      searchLanguage: String,
                      page: Int,
                      pageSize: Int,
                      sort: Sort.Value,
                      fallback: Boolean): Try[api.ConceptSearchResult] = {
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

      executeSearch(withIdIn, language, sort, page, pageSize, fullQuery, fallback)
    }

    def executeSearch(withIdIn: List[Long],
                      language: String,
                      sort: Sort.Value,
                      page: Int,
                      pageSize: Int,
                      queryBuilder: BoolQueryDefinition,
                      fallback: Boolean): Try[api.ConceptSearchResult] = {
      val idFilter = if (withIdIn.isEmpty) None else Some(idsQuery(withIdIn))

      val (languageFilter, searchLanguage) = language match {
        case "" | Language.AllLanguages | "*" =>
          (None, "*")
        case lang =>
          fallback match {
            case true => (None, "*")
            case false => (Some(existsQuery(s"title.$lang")), lang)
          }
      }

      val filters = List(idFilter, languageFilter)
      val filteredSearch = queryBuilder.filter(filters.flatten)

      val (startAt, numResults) = getStartAtAndNumResults(page, pageSize)
      val requestedResultWindow = pageSize * page
      if (requestedResultWindow > DraftApiProperties.ElasticSearchIndexMaxResultWindow) {
        logger.info(s"Max supported results are ${DraftApiProperties.ElasticSearchIndexMaxResultWindow}, user requested $requestedResultWindow")
        Failure(new ResultWindowTooLargeException())
      } else {
        val searchExec =
          search(searchIndex)
            .size(numResults)
            .from(startAt)
            .query(filteredSearch)
            .highlighting(highlight("*"))
            .sortBy(getSortDefinition(sort, searchLanguage))

        e4sClient.execute(searchExec) match {
          case Success(response) =>
            Success(api.ConceptSearchResult(
              response.result.totalHits,
              page,
              numResults,
              if (searchLanguage == "*") Language.AllLanguages else searchLanguage,
              getHits(response.result, language)
            ))
          case Failure(ex) =>
            errorHandler(ex)
        }
      }
    }

    override def scheduleIndexDocuments() = {
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
