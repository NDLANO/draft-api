/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import no.ndla.network.NdlaClient
import no.ndla.draftapi.DraftApiProperties.ArticleApiHost
import no.ndla.draftapi.model.api.ArticleId
import org.json4s.native.Serialization.write

import scala.util.Try
import scalaj.http.Http

case class ArticleApiId(id: Long)

trait ArticleApiClient {
  this: NdlaClient =>
  val ArticleApiClient: ArticleApiClient

  class ArticleApiClient {
    private val InternalEndpoint = s"http://$ArticleApiHost/intern"

    def allocateArticleId: Try[Long] =
      post[ArticleId, String](s"$InternalEndpoint/id/article/allocate", "").map(_.id)

    private def post[A, B <: AnyRef](endpointUrl: String, data: B)(implicit mf: Manifest[A]): Try[A] = {
      implicit val format = org.json4s.DefaultFormats
      ndlaClient.fetch[A](Http(endpointUrl).postData(write(data)))
    }

  }
}
