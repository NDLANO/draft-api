/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import java.util.concurrent.{Executors, TimeUnit}

import no.ndla.draftapi.auth.{Role, User}
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.ContentId
import no.ndla.draftapi.model.domain.{ArticleStatus, ArticleType, Language}
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service._
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService, ConceptIndexService, IndexService}
import org.json4s.ext.EnumNameSerializer
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.swagger.Swagger
import org.scalatra.{InternalServerError, NotFound, Ok}

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

trait InternController {
  this: ReadService
    with WriteService
    with ConverterService
    with DraftRepository
    with IndexService
    with ArticleIndexService
    with ConceptIndexService
    with AgreementIndexService
    with User
    with ArticleApiClient =>
  val internController: InternController

  class InternController(implicit val swagger: Swagger) extends NdlaController {
    protected val applicationDescription = "API for accessing internal functionality in draft API"
    protected implicit override val jsonFormats: Formats =
      org.json4s.DefaultFormats +
        new EnumNameSerializer(ArticleStatus) +
        new EnumNameSerializer(ArticleType)

    post("/index") {
      implicit val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
      val indexResults = for {
        articleIndex <- Future { articleIndexService.indexDocuments }
        conceptIndex <- Future { conceptIndexService.indexDocuments }
        agreementIndex <- Future { agreementIndexService.indexDocuments }
      } yield (articleIndex, conceptIndex, agreementIndex)

      Await.result(indexResults, Duration(1, TimeUnit.MINUTES)) match {
        case (Success(articleResult), Success(conceptResult), Success(agreementIndex)) =>
          val indexTime = math.max(articleResult.millisUsed, conceptResult.millisUsed)
          val result =
            s"Completed indexing of ${articleResult.totalIndexed} articles, ${conceptResult.totalIndexed} concepts and ${agreementIndex.totalIndexed} agreements in $indexTime ms."
          logger.info(result)
          Ok(result)
        case (Failure(articleFail), _, _) =>
          logger.warn(articleFail.getMessage, articleFail)
          InternalServerError(articleFail.getMessage)
        case (_, Failure(conceptFail), _) =>
          logger.warn(conceptFail.getMessage, conceptFail)
          InternalServerError(conceptFail.getMessage)
        case (_, _, Failure(agreementFail)) =>
          logger.warn(agreementFail.getMessage, agreementFail)
          InternalServerError(agreementFail.getMessage)
      }
    }

    get("/ids") {
      draftRepository.getAllIds
    }

    get("/id/:external_id") {
      val externalId = params("external_id")
      draftRepository.getIdFromExternalId(externalId) match {
        case Some(id) => id
        case None     => NotFound()
      }
    }

    get("/articles") {
      val pageNo = intOrDefault("page", 1)
      val pageSize = intOrDefault("page-size", 250)
      val lang = paramOrDefault("language", Language.AllLanguages)
      val fallback = booleanOrDefault("fallback", default = false)

      readService.getArticlesByPage(pageNo, pageSize, lang, fallback)
    }

    post("/articles/publish/") {
      val userInfo = user.getUser
      doOrAccessDenied(userInfo.isAdmin) {
        writeService.publishArticles(userInfo)
      }
    }

    @tailrec
    private def deleteArticleWithRetries(id: Long, maxRetries: Int = 10, retries: Int = 0): Try[ContentId] = {
      articleApiClient.deleteArticle(id) match {
        case Failure(_) if retries <= maxRetries => deleteArticleWithRetries(id, maxRetries, retries + 1)
        case Failure(ex)                         => Failure(ex)
        case Success(x)                          => Success(x)
      }
    }

    delete("/article/:id/") {
      doOrAccessDenied(user.getUser.canWrite) {
        val id = long("id")
        deleteArticleWithRetries(id).flatMap(id => writeService.deleteArticle(id.id)) match {
          case Success(a)  => a
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    post("/concept/:id/publish/") {
      doOrAccessDenied(user.getUser.canWrite) {
        writeService.publishConcept(long("id")) match {
          case Success(s)  => s.id.map(ContentId)
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    delete("/concept/:id/") {
      doOrAccessDenied(user.getUser.canWrite) {
        articleApiClient.deleteConcept(long("id")).flatMap(id => writeService.deleteConcept(id.id)) match {
          case Success(c)  => c
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    post("/empty_article/") {
      doOrAccessDenied(user.getUser.canWrite) {
        val externalId = paramAsListOfString("externalId")
        val externalSubjectIds = paramAsListOfString("externalSubjectId")
        writeService.newEmptyArticle(externalId, externalSubjectIds) match {
          case Success(id) => ContentId(id)
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    post("/empty_concept/") {
      doOrAccessDenied(user.getUser.canWrite) {
        val externalId = paramAsListOfString("externalId")
        writeService.newEmptyConcept(externalId) match {
          case Success(id) => id
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    get("/dump/article") {
      // Dumps Domain articles
      val pageNo = intOrDefault("page", 1)
      val pageSize = intOrDefault("page-size", 250)

      readService.getArticleDomainDump(pageNo, pageSize)
    }

  }
}
