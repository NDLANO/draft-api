/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import java.util.concurrent.Executors

import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.searches.queries.BoolQuery
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.DraftApiProperties.{ElasticSearchScrollKeepAlive, ElasticSearchIndexMaxResultWindow}
import no.ndla.draftapi.integration.Elastic4sClient
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.api.ResultWindowTooLargeException
import no.ndla.draftapi.model.domain._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

trait ArticleSearchService {
  this: Elastic4sClient
    with SearchConverterService
    with SearchService
    with ArticleIndexService
    with SearchConverterService =>
  val articleSearchService: ArticleSearchService

  class ArticleSearchService extends LazyLogging with SearchService[api.ArticleSummary] {
    private val noCopyright = boolQuery().not(termQuery("license", "copyrighted"))

    override val searchIndex: String = DraftApiProperties.DraftSearchIndex

    override def hitToApiModel(hit: String, language: String): api.ArticleSummary =
      searchConverterService.hitAsArticleSummary(hit, language)

    def all(withIdIn: List[Long],
            language: String,
            license: Option[String],
            page: Int,
            pageSize: Int,
            sort: Sort.Value,
            articleTypes: Seq[String],
            fallback: Boolean): Try[SearchResult[api.ArticleSummary]] =
      executeSearch(withIdIn, language, license, sort, page, pageSize, boolQuery(), articleTypes, fallback)

    def matchingQuery(query: String,
                      withIdIn: List[Long],
                      searchLanguage: String,
                      license: Option[String],
                      page: Int,
                      pageSize: Int,
                      sort: Sort.Value,
                      articleTypes: Seq[String],
                      fallback: Boolean): Try[SearchResult[api.ArticleSummary]] = {

      val language = if (searchLanguage == Language.AllLanguages) "*" else searchLanguage
      val titleSearch = simpleStringQuery(query).field(s"title.$language", 3)
      val introSearch = simpleStringQuery(query).field(s"introduction.$language", 2)
      val contentSearch = simpleStringQuery(query).field(s"content.$language", 1)
      val tagSearch = simpleStringQuery(query).field(s"tags.$language", 2)
      val notesSearch = simpleStringQuery(query).field("notes", 1)

      val fullQuery = boolQuery()
        .must(
          boolQuery()
            .should(
              titleSearch,
              introSearch,
              contentSearch,
              tagSearch,
              notesSearch
            )
        )

      executeSearch(withIdIn, language, license, sort, page, pageSize, fullQuery, articleTypes, fallback)
    }

    def executeSearch(withIdIn: List[Long],
                      language: String,
                      license: Option[String],
                      sort: Sort.Value,
                      page: Int,
                      pageSize: Int,
                      queryBuilder: BoolQuery,
                      articleTypes: Seq[String],
                      fallback: Boolean): Try[SearchResult[api.ArticleSummary]] = {

      val articleTypesFilter =
        if (articleTypes.nonEmpty) Some(constantScoreQuery(termsQuery("articleType", articleTypes))) else None

      val licenseFilter = license match {
        case None      => Some(noCopyright)
        case Some(lic) => Some(termQuery("license", lic))
      }

      val idFilter = if (withIdIn.isEmpty) None else Some(idsQuery(withIdIn))

      val (languageFilter, searchLanguage) = language match {
        case "" | Language.AllLanguages =>
          (None, "*")
        case lang =>
          if (fallback)
            (None, "*")
          else
            (Some(existsQuery(s"title.$lang")), lang)
      }

      val filters = List(licenseFilter, idFilter, languageFilter, articleTypesFilter)
      val filteredSearch = queryBuilder.filter(filters.flatten)

      val (startAt, numResults) = getStartAtAndNumResults(page, pageSize)
      val requestedResultWindow = pageSize * page
      if (requestedResultWindow > ElasticSearchIndexMaxResultWindow) {
        logger.info(
          s"Max supported results are $ElasticSearchIndexMaxResultWindow, user requested $requestedResultWindow")
        Failure(new ResultWindowTooLargeException())
      } else {
        val searchToExecute = search(searchIndex)
          .size(numResults)
          .from(startAt)
          .query(filteredSearch)
          .highlighting(highlight("*"))
          .sortBy(getSortDefinition(sort, searchLanguage))

        val searchWithScroll =
          if (startAt != 0) { searchToExecute } else { searchToExecute.scroll(ElasticSearchScrollKeepAlive) }

        e4sClient.execute(searchWithScroll) match {
          case Success(response) =>
            Success(
              SearchResult(
                response.result.totalHits,
                Some(page),
                numResults,
                if (searchLanguage == "*") Language.AllLanguages else searchLanguage,
                getHits(response.result, language),
                response.result.scrollId
              ))
          case Failure(ex) =>
            errorHandler(ex)
        }
      }
    }

    override def scheduleIndexDocuments(): Unit = {
      implicit val ec: ExecutionContextExecutorService =
        ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
      val f = Future {
        articleIndexService.indexDocuments
      }

      f.failed.foreach(t => logger.warn("Unable to create index: " + t.getMessage, t))
      f.foreach {
        case Success(reindexResult) =>
          logger.info(
            s"Completed indexing of ${reindexResult.totalIndexed} articles in ${reindexResult.millisUsed} ms.")
        case Failure(ex) => logger.warn(ex.getMessage, ex)
      }
    }

  }
}
