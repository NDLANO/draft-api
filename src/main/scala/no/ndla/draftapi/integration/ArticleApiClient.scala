/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import no.ndla.draftapi.DraftApiProperties.ArticleApiHost
import no.ndla.draftapi.model.api
import no.ndla.draftapi.model.api.{ArticleApiValidationError, ContentId}
import no.ndla.draftapi.model.domain.Article
import no.ndla.network.NdlaClient
import no.ndla.network.model.HttpRequestException
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.json4s.native.Serialization.write
import org.json4s.jackson.JsonMethods.parse

import scala.util.{Failure, Success, Try}
import scalaj.http.Http

case class ArticleApiId(id: Long)

trait ArticleApiClient {
  this: NdlaClient =>
  val ArticleApiClient: ArticleApiClient

  class ArticleApiClient {
    private val InternalEndpoint = s"http://$ArticleApiHost/intern"

    def allocateArticleId(externalId: Option[String], externalSubjectIds: Seq[String]): Try[Long] = {
      implicit val format = org.json4s.DefaultFormats
      val params = externalId match {
        case Some(nid) => Seq("external-id" -> nid, "external-subject-id" -> externalSubjectIds.mkString(","))
        case None => Seq.empty
      }
      post[ContentId](s"$InternalEndpoint/id/article/allocate", params: _*).map(_.id)
    }

    def allocateConceptId(externalId: Option[String]): Try[Long] = {
      implicit val format = org.json4s.DefaultFormats
      val params = externalId match {
        case Some(nid) => Seq("external-id" -> nid)
        case None => Seq.empty
      }
      post[ContentId](s"$InternalEndpoint/id/concept/allocate", params: _*).map(_.id)
    }

    def getValidationErrors(article: Article): Try[Set[ValidationMessage]] = {
      implicit val formats = Article.formats
      postWithData[Set[ValidationMessage], Article](s"$InternalEndpoint/validate/article", article)
    }

    def updateConcept(id: Long, concept: api.ArticleApiConcept): Try[api.ArticleApiConcept] = {
      implicit val format = org.json4s.DefaultFormats
      postWithData[api.ArticleApiConcept, api.ArticleApiConcept](s"$InternalEndpoint/concept/$id", concept)
    }

    def updateArticle(id: Long, article: api.ArticleApiArticle): Try[api.ArticleApiArticle] = {
      implicit val format = org.json4s.DefaultFormats
      postWithData[api.ArticleApiArticle, api.ArticleApiArticle](s"$InternalEndpoint/article/$id", article)
    }

    def validateArticle(article: api.ArticleApiArticle): Try[api.ArticleApiArticle] = {
      implicit val format = org.json4s.DefaultFormats
      postWithData[api.ArticleApiArticle, api.ArticleApiArticle](s"$InternalEndpoint/validate/article", article) match {
        case Failure(ex: HttpRequestException) =>
          val validationError = ex.httpResponse.map(r => parse(r.body).extract[ArticleApiValidationError])
          Failure(new ValidationException("Failed to validate article in article-api", validationError.map(_.messages).getOrElse(Seq.empty)))
        case x => x
      }
    }

    private def post[A](endpointUrl: String, params: (String, String)*)(implicit mf: Manifest[A], format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(endpointUrl).method("POST").params(params.toMap))
    }

    private def postWithData[A, B <: AnyRef](endpointUrl: String, data: B)(implicit mf: Manifest[A], format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("POST")
          .header("content-type", "application/json")
      )
    }

  }
}
