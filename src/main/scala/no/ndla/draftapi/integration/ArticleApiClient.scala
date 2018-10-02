/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import no.ndla.draftapi.DraftApiProperties.ArticleApiHost
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.api.{ArticleApiValidationError, ContentId}
import no.ndla.draftapi.model.domain.Article
import no.ndla.draftapi.service.ConverterService
import no.ndla.network.NdlaClient
import no.ndla.network.model.HttpRequestException
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.json4s.native.Serialization.write
import org.json4s.jackson.JsonMethods.parse

import scala.util.{Failure, Success, Try}
import scalaj.http.Http

case class ArticleApiId(id: Long)

trait ArticleApiClient {
  this: NdlaClient with ConverterService =>
  val articleApiClient: ArticleApiClient

  class ArticleApiClient {
    private val InternalEndpoint = s"http://$ArticleApiHost/intern"
    private val deleteTimeout = 1000 * 10 // 10 seconds

    def allocateArticleId(externalIds: List[String], externalSubjectIds: Seq[String]): Try[Long] = {
      implicit val format = org.json4s.DefaultFormats

      val params = externalIds match {
        case nids if nids.nonEmpty =>
          Seq("external-id" -> nids.mkString(","), "external-subject-id" -> externalSubjectIds.mkString(","))
        case _ => Seq.empty
      }
      post[ContentId](s"$InternalEndpoint/id/article/allocate", params: _*).map(_.id)
    }

    def allocateConceptId(externalIds: List[String]): Try[Long] = {
      implicit val format = org.json4s.DefaultFormats
      val params = externalIds match {
        case nids if nids.nonEmpty => Seq("external-id" -> nids.mkString(","))
        case _                     => Seq.empty
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

    def deleteConcept(id: Long): Try[ContentId] = {
      implicit val format = org.json4s.DefaultFormats
      delete[ContentId](s"$InternalEndpoint/concept/$id/")
    }

    def updateArticle(id: Long, article: domain.Article, externalIds: List[String]): Try[domain.Article] = {
      implicit val format = org.json4s.DefaultFormats

      val articleApiArticle = converterService.toArticleApiArticle(article)
      postWithData[api.ArticleApiArticle, api.ArticleApiArticle](s"$InternalEndpoint/article/$id",
                                                                 articleApiArticle,
                                                                 "external-id" -> externalIds.mkString(","))
        .map(_ => article)
    }

    def unpublishArticle(article: domain.Article): Try[domain.Article] = {
      implicit val format = org.json4s.DefaultFormats
      val id = article.id.get
      post[ContentId](s"$InternalEndpoint/article/$id/unpublish/").map(_ => article)
    }

    def deleteArticle(id: Long): Try[ContentId] = {
      implicit val format = org.json4s.DefaultFormats
      delete[ContentId](s"$InternalEndpoint/article/$id/")
    }

    def validateArticle(article: api.ArticleApiArticle, importValidate: Boolean): Try[api.ArticleApiArticle] = {
      implicit val format = org.json4s.DefaultFormats
      postWithData[api.ArticleApiArticle, api.ArticleApiArticle](s"$InternalEndpoint/validate/article",
                                                                 article,
                                                                 ("import_validate", importValidate.toString)) match {
        case Failure(ex: HttpRequestException) =>
          val validationError = ex.httpResponse.map(r => parse(r.body).extract[ArticleApiValidationError])
          Failure(
            new ValidationException("Failed to validate article in article-api",
                                    validationError.map(_.messages).getOrElse(Seq.empty)))
        case x => x
      }
    }

    private def post[A](endpointUrl: String, params: (String, String)*)(implicit mf: Manifest[A],
                                                                        format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(endpointUrl).method("POST").params(params.toMap))
    }

    private def delete[A](endpointUrl: String, params: (String, String)*)(implicit mf: Manifest[A],
                                                                          format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](
        Http(endpointUrl).method("DELETE").params(params.toMap).timeout(deleteTimeout, deleteTimeout))
    }

    private def postWithData[A, B <: AnyRef](endpointUrl: String, data: B, params: (String, String)*)(
        implicit mf: Manifest[A],
        format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("POST")
          .params(params.toMap)
          .header("content-type", "application/json")
      )
    }

  }
}
