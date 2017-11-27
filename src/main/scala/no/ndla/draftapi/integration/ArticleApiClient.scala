/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import no.ndla.draftapi.DraftApiProperties.ArticleApiHost
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.api.ArticleId
import no.ndla.draftapi.model.domain.Article
import no.ndla.network.NdlaClient
import no.ndla.validation.ValidationMessage
import org.json4s.native.Serialization.write

import scala.util.Try
import scalaj.http.Http

case class ArticleApiId(id: Long)

trait ArticleApiClient {
  this: NdlaClient =>
  val ArticleApiClient: ArticleApiClient

  class ArticleApiClient {
    private val InternalEndpoint = s"http://$ArticleApiHost/intern"

    def allocateArticleId: Try[Long] = {
      implicit val format = org.json4s.DefaultFormats
      post[ArticleId, String](s"$InternalEndpoint/id/article/allocate", "").map(_.id)
    }

    def getValidationErrors(article: Article): Try[Set[ValidationMessage]] = {
      implicit val formats = Article.formats
      post[Set[ValidationMessage], Article](s"$InternalEndpoint/validate/article", article)
    }

    def updateArticle(id: Long, article: api.ArticleApiArticle): Try[api.ArticleApiArticle] = {
      implicit val format = org.json4s.DefaultFormats
      post[api.ArticleApiArticle, api.ArticleApiArticle](s"$InternalEndpoint/article/$id", article)
    }

    def validateArticle(article: api.ArticleApiArticle): Try[api.ArticleApiArticle] = {
      implicit val format = org.json4s.DefaultFormats
      post[api.ArticleApiArticle, api.ArticleApiArticle](s"$InternalEndpoint/validate/article", article)
    }

    private def post[A, B <: AnyRef](endpointUrl: String, data: B)(implicit mf: Manifest[A], format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetch[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("POST")
          .header("content-type", "application/json")
      )
    }

  }
}
