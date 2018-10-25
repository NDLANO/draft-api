/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration
import java.util.concurrent.Executors

import cats.Traverse
import com.typesafe.scalalogging.LazyLogging
import no.ndla.network.NdlaClient
import no.ndla.network.model.HttpRequestException
import no.ndla.draftapi.DraftApiProperties.Domain
import no.ndla.draftapi.model.domain.{Article, ArticleTitle, Language}
import org.json4s.{DefaultFormats, Formats}
import scalaj.http.Http
import org.json4s.jackson.Serialization.write

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}
import cats.implicits._

trait TaxonomyApiClient {
  this: NdlaClient =>
  val taxonomyApiClient: TaxonomyApiClient

  class TaxonomyApiClient extends LazyLogging {
    private val TaxonomyApiEndpoint = s"$Domain/taxonomy/v1"
    private val taxonomyTimeout = 20 * 1000 // 20 Seconds
    private val timeoutDur = Duration(taxonomyTimeout, "seconds")
    implicit val formats: DefaultFormats.type = DefaultFormats

    def queryResource(articleId: Long): Try[List[Resource]] = {
      get[List[Resource]](s"$TaxonomyApiEndpoint/queries/resources", "contentURI" -> s"urn:article:$articleId")
    }

    def queryTopic(articleId: Long): Try[List[Topic]] = {
      get[List[Topic]](s"$TaxonomyApiEndpoint/queries/topics", "contentURI" -> s"urn:article:$articleId")
    }

    def updateTaxonomyIfExists(articleId: Long, article: Article): Try[Long] = {
      // Get places to update names
      val resourcesAndTopics = for {
        resources <- queryResource(articleId)
        topics <- queryTopic(articleId)
      } yield (resources, topics)

      resourcesAndTopics match {
        case Success((res, top)) =>
          Language.findByLanguageOrBestEffort(article.title, Language.DefaultLanguage) match {
            case Some(defaultTitle) =>
              // Threadpool with size according to number of topics and resources
              val threadPoolSize = math.max(3, res.size * 2 + top.size * 2)
              implicit val ec: ExecutionContextExecutor =
                ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadPoolSize))

              // Do the actual updates
              val resources = res.map(r => updateResourceTitleAndTranslations(r, defaultTitle, article))
              val topics = top.map(t => updateTopicTitleAndTranslations(t, defaultTitle, article))

              // Find whether there was an error or not
              val resourceResult = resources.collectFirst { case Failure(ex) => ex } match {
                case Some(ex) => Failure(ex)
                case None     => Success(articleId)
              }

              val topicResult = topics.collectFirst { case Failure(ex) => ex } match {
                case Some(ex) => Failure(ex)
                case None     => Success(articleId)
              }

              // Return either error
              (resourceResult, topicResult) match {
                case (Success(r), Success(t)) => Success(r)
                case (Failure(ex), _)         => Failure(ex)
                case (_, Failure(ex))         => Failure(ex)
              }
            case None => Failure(new RuntimeException("This is a bug, no name was found for published article."))
          }
        case Failure(ex) => Failure(ex)
      }
    }

    private def updateResourceTitleAndTranslations(res: Resource, defaultTitle: ArticleTitle, article: Article)(
        implicit ec: ExecutionContext) = {
      val resourceResult = Try(Await.result(updateResource(res.copy(name = defaultTitle.title)), timeoutDur)).flatten
      val translationResult = updateResourceTranslations(res.id, article.title)

      (resourceResult, translationResult) match {
        case (Success(s1), Success(_)) => Success(s1)
        case (Failure(ex), _)          => Failure(ex)
        case (_, Failure(ex))          => Failure(ex)
      }
    }

    private def updateTopicTitleAndTranslations(top: Topic, defaultTitle: ArticleTitle, article: Article)(
        implicit ec: ExecutionContext) = {
      val topicResult = Try(Await.result(updateTopic(top.copy(name = defaultTitle.title)), timeoutDur)).flatten
      val translationResult = updateTopicTranslations(top.id, article.title)

      (topicResult, translationResult) match {
        case (Success(s1), Success(_)) => Success(s1)
        case (Failure(ex), _)          => Failure(ex)
        case (_, Failure(ex))          => Failure(ex)
      }
    }

    private def updateResourceTranslations(resId: String, titles: Seq[ArticleTitle])(implicit ec: ExecutionContext) = {
      val fut = Future.sequence(titles.map(t => updateResourceTranslation(resId, t.language, t.title)))
      Try(Await.result(fut, timeoutDur)).flatMap(translations => Traverse[List].sequence(translations.toList))
    }

    private def updateTopicTranslations(topicId: String, titles: Seq[ArticleTitle])(implicit ec: ExecutionContext) = {
      val fut = Future.sequence(titles.map(t => updateTopicTranslation(topicId, t.language, t.title)))
      Try(Await.result(fut, timeoutDur)).flatMap(translations => Traverse[List].sequence(translations.toList))
    }

    private[integration] def updateResourceTranslation(resourceId: String, lang: String, name: String)(
        implicit ec: ExecutionContext) =
      putRaw(s"$TaxonomyApiEndpoint/resources/$resourceId/translations/$lang", Translation(name))

    private[integration] def updateTopicTranslation(topicId: String, lang: String, name: String)(
        implicit ec: ExecutionContext) =
      putRaw(s"$TaxonomyApiEndpoint/topics/$topicId/translations/$lang", Translation(name))

    private[integration] def updateResource(resource: Resource)(implicit formats: Formats, ec: ExecutionContext) = {
      putRaw[Resource](s"$TaxonomyApiEndpoint/resources/${resource.id}", resource)
    }

    private[integration] def updateTopic(topic: Topic)(implicit formats: Formats, ec: ExecutionContext) = {
      putRaw[Topic](s"$TaxonomyApiEndpoint/topics/${topic.id}", topic)
    }

    private def get[A](url: String, params: (String, String)*)(implicit mf: Manifest[A]): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(url).timeout(taxonomyTimeout, taxonomyTimeout).params(params))
    }

    private[integration] def putRaw[B <: AnyRef](url: String, data: B, params: (String, String)*)(
        implicit formats: org.json4s.Formats,
        ec: ExecutionContext): Future[Try[B]] = {
      Future {
        logger.info(s"Doing call to $url")
        ndlaClient.fetchRawWithForwardedAuth(
          Http(url)
            .put(write(data))
            .header("content-type", "application/json")
            .params(params)
        ) match {
          case Success(_)  => Success(data)
          case Failure(ex) => Failure(ex)
        }
      }
    }
  }
}

sealed trait Taxonomy { val id: String; val name: String; val contentUri: Option[String] }
case class UpdateResource(contentUri: String, name: String)
case class Resource(id: String, name: String, contentUri: Option[String]) extends Taxonomy
case class Topic(id: String, name: String, contentUri: Option[String]) extends Taxonomy
case class Translation(name: String)
