/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import java.util.concurrent.{Executors, TimeUnit}
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.auth.User
import no.ndla.draftapi.integration.ArticleApiClient
import no.ndla.draftapi.model.api.{ContentId, NotFoundException}
import no.ndla.draftapi.model.domain.{ArticleStatus, ArticleType, Language}
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.domain.Article.jsonEncoder
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service._
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService, IndexService, TagIndexService}
import org.json4s.Formats
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
    with TagIndexService
    with AgreementIndexService
    with User
    with ArticleApiClient =>
  val internController: InternController

  class InternController(implicit val swagger: Swagger) extends NdlaController {
    protected val applicationDescription = "API for accessing internal functionality in draft API"
    protected implicit override val jsonFormats: Formats = jsonEncoder

    post("/index") {
      implicit val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
      val indexResults = for {
        articleIndex <- Future { articleIndexService.indexDocuments }
        agreementIndex <- Future { agreementIndexService.indexDocuments }
        tagIndex <- Future { tagIndexService.indexDocuments }
      } yield (articleIndex, agreementIndex, tagIndex)

      Await.result(indexResults, Duration(10, TimeUnit.MINUTES)) match {
        case (Success(articleResult), Success(agreementResult), Success(tagResult)) =>
          val indexTime = math.max(math.max(articleResult.millisUsed, agreementResult.millisUsed), tagResult.millisUsed)
          val result =
            s"Completed indexing of ${articleResult.totalIndexed} articles, and ${agreementResult.totalIndexed} agreements in $indexTime ms."
          logger.info(result)
          Ok(result)
        case (Failure(articleFail), _, _) =>
          logger.warn(articleFail.getMessage, articleFail)
          InternalServerError(articleFail.getMessage)
        case (_, Failure(agreementFail), _) =>
          logger.warn(agreementFail.getMessage, agreementFail)
          InternalServerError(agreementFail.getMessage)
        case (_, _, Failure(tagFail)) =>
          logger.warn(tagFail.getMessage, tagFail)
          InternalServerError(tagFail.getMessage)
      }
    }

    delete("/index") {
      implicit val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
      def pluralIndex(n: Int) = if (n == 1) "1 index" else s"$n indexes"

      val indexes = for {
        articleIndex <- Future { articleIndexService.findAllIndexes(DraftApiProperties.DraftSearchIndex) }
        agreementIndex <- Future { agreementIndexService.findAllIndexes(DraftApiProperties.AgreementSearchIndex) }
        tagIndex <- Future { tagIndexService.findAllIndexes(DraftApiProperties.DraftTagSearchIndex) }
      } yield (articleIndex, agreementIndex, tagIndex)

      val deleteResults: Seq[Try[_]] = Await.result(indexes, Duration(10, TimeUnit.MINUTES)) match {
        case (Failure(articleFail), _, _)   => halt(status = 500, body = articleFail.getMessage)
        case (_, Failure(agreementFail), _) => halt(status = 500, body = agreementFail.getMessage)
        case (_, _, Failure(tagFail))       => halt(status = 500, body = tagFail.getMessage)
        case (Success(articleIndexes), Success(agreementIndexes), Success(tagIndexes)) => {
          val articleDeleteResults = articleIndexes.map(index => {
            logger.info(s"Deleting article index $index")
            articleIndexService.deleteIndexWithName(Option(index))
          })
          val agreementDeleteResults = agreementIndexes.map(index => {
            logger.info(s"Deleting agreement index $index")
            agreementIndexService.deleteIndexWithName(Option(index))
          })
          val tagDeleteResults = tagIndexes.map(index => {
            logger.info(s"Deleting tag index $index")
            tagIndexService.deleteIndexWithName(Option(index))
          })
          articleDeleteResults ++ agreementDeleteResults ++ tagDeleteResults
        }
      }

      val (errors, successes) = deleteResults.partition(_.isFailure)
      if (errors.nonEmpty) {
        val message = s"Failed to delete ${pluralIndex(errors.length)}: " +
          s"${errors.map(_.failed.get.getMessage).mkString(", ")}. " +
          s"${pluralIndex(successes.length)} were deleted successfully."
        halt(status = 500, body = message)
      } else {
        Ok(body = s"Deleted ${pluralIndex(successes.length)}")
      }

    }

    get("/ids") {
      paramOrNone("status").map(ArticleStatus.valueOfOrError) match {
        case Some(Success(status)) => draftRepository.idsWithStatus(status).getOrElse(List.empty)
        case Some(Failure(ex))     => errorHandler(ex)
        case None                  => draftRepository.getAllIds
      }
    }

    get("/import-id/:external_id") {
      val articleId = params("external_id")
      readService.importIdOfArticle(articleId) match {
        case Some(ids) => Ok(ids)
        case _         => NotFound()
      }
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

    get("/dump/article/?") {
      // Dumps all domain articles
      val pageNo = intOrDefault("page", 1)
      val pageSize = intOrDefault("page-size", 250)

      readService.getArticleDomainDump(pageNo, pageSize)
    }

    get("/dump/article/:id") {
      // Dumps one domain article
      val id = long("id")
      draftRepository.withId(id) match {
        case Some(article) => Ok(article)
        case None          => errorHandler(NotFoundException(s"Could not find draft with id: '$id"))
      }
    }

    post("/dump/article/?") {
      extract[domain.Article](request.body) match {
        case Failure(ex) => errorHandler(ex)
        case Success(article) =>
          writeService.insertDump(article) match {
            case Failure(ex)       => errorHandler(ex)
            case Success(inserted) => Ok(inserted)
          }
      }

    }

  }
}
