/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import com.netaporter.uri.Uri
import no.ndla.draftapi.DraftApiProperties.LearningpathApiHost
import no.ndla.draftapi.caching.Memoize
import no.ndla.draftapi.service.ConverterService
import no.ndla.network.NdlaClient
import scalaj.http.Http

import scala.util.{Failure, Success, Try}

trait LearningpathApiClient {
  this: NdlaClient with ConverterService =>
  val learningpathApiClient: LearningpathApiClient

  class LearningpathApiClient {
    implicit val format = org.json4s.DefaultFormats
    private val InternalEndpoint = s"http://$LearningpathApiHost/intern"

    val getLearningpaths = Memoize[Try[Seq[LearningPath]]](
      () => {
        val pageSize = 100
        val firstPage = getLearningpathDump(1, pageSize)

        firstPage match {
          case Success(first) =>
            val numPagesToFetch = (first.totalCount - pageSize) / pageSize + 1

            val allPages = Seq(firstPage.map(_.results)) ++ (2 to numPagesToFetch)
              .map(getLearningpathDump(_, pageSize).map(page => page.results))
            Try(allPages.map(_.get)).map(_.flatten)
          case Failure(ex) => Failure(ex)
        }
      },
      shouldCacheResult = (toCache: Try[_]) => toCache.isSuccess
    )

    private def getLearningpathDump(pageNo: Int, pageSize: Int): Try[LearningPathDomainDump] =
      get[LearningPathDomainDump](s"$InternalEndpoint/dump/learningpath",
                                  "page" -> s"$pageNo",
                                  "page-size" -> s"$pageSize")

    private def get[A](endpointUrl: String, params: (String, String)*)(implicit mf: Manifest[A],
                                                                       format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(endpointUrl).method("GET").params(params.toMap))
    }

  }
}
case class LearningPathDomainDump(totalCount: Int, page: Int, pageSize: Int, results: Seq[LearningPath])
case class LearningPath(id: Option[Long], learningsteps: Seq[LearningStep])
case class LearningStep(id: Option[Long], embedUrl: Seq[EmbedUrl])
case class EmbedUrl(url: String, language: String, embedType: String)
