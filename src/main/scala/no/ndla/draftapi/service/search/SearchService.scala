/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service.search

import java.lang.Math.max

import com.typesafe.scalalogging.LazyLogging
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.search.SearchResponse
import com.sksamuel.elastic4s.searches.sort.{FieldSortDefinition, SortOrder}
import no.ndla.draftapi.DraftApiProperties.{DefaultPageSize, MaxPageSize}
import no.ndla.draftapi.integration.Elastic4sClient
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.domain._

import scala.util.{Failure, Success}

trait SearchService {
  this: Elastic4sClient with SearchConverterService with LazyLogging =>

  trait SearchService[T] {
    val searchIndex: String

    def hitToApiModel(hit: String, language: String): T

    def getHits(response: SearchResponse, language: String, hitToApi:(String, String) => T): Seq[T] = {
      response.totalHits match {
        case count if count > 0 =>
          val resultArray = response.hits.hits

          resultArray.map(result => {
            val matchedLanguage = language match {
              case Language.AllLanguages | "*" =>
                searchConverterService.getLanguageFromHit(result).getOrElse(language)
              case _ => language
            }

            hitToApi(result.sourceAsString, matchedLanguage)
          })
        case _ => Seq()
      }
    }

    def getSortDefinition(sort: Sort.Value, language: String): FieldSortDefinition = {
      val sortLanguage = language match {
        case domain.Language.NoLanguage => domain.Language.DefaultLanguage
        case _ => language
      }

      sort match {
        case (Sort.ByTitleAsc) =>
          language match {
            case "*" | Language.AllLanguages => fieldSort("defaultTitle").order(SortOrder.ASC).missing("_last")
            case _ => fieldSort(s"title.$sortLanguage.raw").nestedPath("title").order(SortOrder.ASC).missing("_last")
          }
        case (Sort.ByTitleDesc) =>
          language match {
            case "*" | Language.AllLanguages => fieldSort("defaultTitle").order(SortOrder.DESC).missing("_last")
            case _ => fieldSort(s"title.$sortLanguage.raw").nestedPath("title").order(SortOrder.DESC).missing("_last")
          }
        case (Sort.ByRelevanceAsc) => fieldSort("_score").order(SortOrder.ASC)
        case (Sort.ByRelevanceDesc) => fieldSort("_score").order(SortOrder.DESC)
        case (Sort.ByLastUpdatedAsc) => fieldSort("lastUpdated").order(SortOrder.ASC).missing("_last")
        case (Sort.ByLastUpdatedDesc) => fieldSort("lastUpdated").order(SortOrder.DESC).missing("_last")
        case (Sort.ByIdAsc) => fieldSort("id").order(SortOrder.ASC).missing("_last")
        case (Sort.ByIdDesc) => fieldSort("id").order(SortOrder.DESC).missing("_last")
      }
    }

    def getSortDefinition(sort: Sort.Value): FieldSortDefinition = {
      sort match {
        case (Sort.ByTitleAsc) => fieldSort("title.raw").order(SortOrder.ASC).missing("_last")
        case (Sort.ByTitleDesc) => fieldSort("title.raw").order(SortOrder.DESC).missing("_last")
        case (Sort.ByRelevanceAsc) => fieldSort("_score").order(SortOrder.ASC)
        case (Sort.ByRelevanceDesc) => fieldSort("_score").order(SortOrder.DESC)
        case (Sort.ByLastUpdatedAsc) => fieldSort("lastUpdated").order(SortOrder.ASC).missing("_last")
        case (Sort.ByLastUpdatedDesc) => fieldSort("lastUpdated").order(SortOrder.DESC).missing("_last")
        case (Sort.ByIdAsc) => fieldSort("id").order(SortOrder.ASC).missing("_last")
        case (Sort.ByIdDesc) => fieldSort("id").order(SortOrder.DESC).missing("_last")
      }
    }

    def countDocuments: Long = {
      val response = e4sClient.execute{
        catCount(searchIndex)
      }

      response match {
        case Success(resp) => resp.result.count
        case Failure(_) => 0
      }
    }

    def getStartAtAndNumResults(page: Int, pageSize: Int): (Int, Int) = {
      val numResults = max(pageSize.min(MaxPageSize), 0)
      val startAt = (page - 1).max(0) * numResults

      (startAt, numResults)
    }

  }
}
