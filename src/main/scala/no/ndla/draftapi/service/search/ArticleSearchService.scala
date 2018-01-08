/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service.search

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.integration.Elastic4sClient
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.api.ResultWindowTooLargeException
import no.ndla.draftapi.model.domain._
import no.ndla.draftapi.service.ConverterService
import no.ndla.network.ApplicationUrl
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.searches.ScoreMode
import com.sksamuel.elastic4s.searches.queries.{BoolQueryDefinition, QueryDefinition}
import org.elasticsearch.ElasticsearchException
import org.elasticsearch.index.IndexNotFoundException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait ArticleSearchService {
  this: Elastic4sClient with SearchConverterService with SearchService with ArticleIndexService with SearchConverterService =>
  val articleSearchService: ArticleSearchService

  class ArticleSearchService extends LazyLogging with SearchService[api.ArticleSummary] {
    private val noCopyright = boolQuery().not(termQuery("license", "copyrighted"))

    override val searchIndex: String = DraftApiProperties.DraftSearchIndex

    override def hitToApiModel(hit: String, language: String): api.ArticleSummary =
      searchConverterService.hitAsArticleSummary(hit, language)

    def all(withIdIn: List[Long], language: String, license: Option[String], page: Int, pageSize: Int, sort: Sort.Value, articleTypes: Seq[String]): api.SearchResult = {
      executeSearch(withIdIn, language, license, sort, page, pageSize, boolQuery(), articleTypes)
    }

    def matchingQuery(query: String, withIdIn: List[Long], searchLanguage: String, license: Option[String], page: Int, pageSize: Int, sort: Sort.Value, articleTypes: Seq[String]): api.SearchResult = {
      val language = if (searchLanguage == Language.AllLanguages) "*" else searchLanguage
      val titleSearch = simpleStringQuery(query).field(s"title.$language", 1)
      val introSearch = simpleStringQuery(query).field(s"introduction.$language", 1)
      val contentSearch = simpleStringQuery(query).field(s"content.$language", 1)
      val tagSearch = simpleStringQuery(query).field(s"tags.$language", 1)

      val hi = highlight("*").preTag("").postTag("").numberOfFragments(0)
      val ih = innerHits("inner_hits").highlighting(hi)

      val fullQuery = boolQuery()
        .must(
          boolQuery()
            .should(
              nestedQuery("title", titleSearch).scoreMode(ScoreMode.Avg).boost(2).inner(ih),
              nestedQuery("introduction", introSearch).scoreMode(ScoreMode.Avg).boost(2).inner(ih),
              nestedQuery("content", contentSearch).scoreMode(ScoreMode.Avg).boost(1).inner(ih),
              nestedQuery("tags", tagSearch).scoreMode(ScoreMode.Avg).boost(2).inner(ih)
            )
        )

      executeSearch(withIdIn, language, license, sort, page, pageSize, fullQuery, articleTypes)
    }

    def executeSearch(withIdIn: List[Long], language: String, license: Option[String], sort: Sort.Value, page: Int, pageSize: Int, queryBuilder: BoolQueryDefinition, articleTypes: Seq[String]): api.SearchResult = {

      val articleTypesFilter = if (articleTypes.nonEmpty) Some(constantScoreQuery(termsQuery("articleType", articleTypes))) else None

      val licenseFilter = license match {
        case None => Some(noCopyright)
        case Some(lic) => Some(termQuery("license", lic))
      }

      val idFilter = if (withIdIn.isEmpty) None else Some(idsQuery(withIdIn))

      val (languageFilter, searchLanguage) = language match {
        case "" | Language.AllLanguages =>
          (None, "*")
        case lang =>
          (Some(nestedQuery("title", existsQuery(s"title.$lang")).scoreMode(ScoreMode.Avg)), lang)
      }

      val filters = List(licenseFilter, idFilter, languageFilter, articleTypesFilter)
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
          .sortBy(getSortDefinition(sort, searchLanguage))
      } match {
        case Success(response) =>
          api.SearchResult(response.result.totalHits, page, numResults, language, getHits(response.result, language, hitToApiModel))
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
      val f = Future {
        articleIndexService.indexDocuments
      }

      f.failed.foreach(t => logger.warn("Unable to create index: " + t.getMessage, t))
      f.foreach {
        case Success(reindexResult) => logger.info(s"Completed indexing of ${reindexResult.totalIndexed} documents in ${reindexResult.millisUsed} ms.")
        case Failure(ex) => logger.warn(ex.getMessage, ex)
      }
    }

  }
}
