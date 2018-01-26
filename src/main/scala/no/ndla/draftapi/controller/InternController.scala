/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.controller

import java.util.concurrent.TimeUnit

import no.ndla.draftapi.auth.Role
import no.ndla.draftapi.model.api.ContentId
import no.ndla.draftapi.model.domain.Language
import no.ndla.draftapi.repository.DraftRepository
import no.ndla.draftapi.service._
import no.ndla.draftapi.service.search.{AgreementIndexService, ArticleIndexService, ConceptIndexService, IndexService}
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.{InternalServerError, NotFound, Ok}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

trait InternController {
  this: ReadService
    with WriteService
    with ConverterService
    with DraftRepository
    with IndexService
    with ArticleIndexService
    with ConceptIndexService
    with AgreementIndexService
    with Role =>
  val internController: InternController

  class InternController extends NdlaController {

    protected implicit override val jsonFormats: Formats = DefaultFormats

    post("/index") {
      val indexResults = for {
        articleIndex <- Future { articleIndexService.indexDocuments }
        conceptIndex <- Future { conceptIndexService.indexDocuments }
        agreementIndex <- Future { agreementIndexService.indexDocuments }
      } yield (articleIndex, conceptIndex, agreementIndex)


      Await.result(indexResults, Duration(1, TimeUnit.MINUTES)) match {
        case (Success(articleResult), Success(conceptResult), Success(agreementIndex)) =>
          val indexTime = math.max(articleResult.millisUsed, conceptResult.millisUsed)
          val result = s"Completed indexing of ${articleResult.totalIndexed} articles, ${conceptResult.totalIndexed} concepts and ${agreementIndex.totalIndexed} agreements in $indexTime ms."
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
        case None => NotFound()
      }
    }

    get("/articles") {
      val pageNo = intOrDefault("page", 1)
      val pageSize = intOrDefault("page-size", 250)
      val lang = paramOrDefault("language", Language.AllLanguages)

      readService.getArticlesByPage(pageNo, pageSize, lang)
    }

    post("/articles/publish/?") {
      authRole.assertHasPublishPermission()
      writeService.publishArticles()
    }

    post("/article/:id/publish/?") {
      authRole.assertHasPublishPermission()
      val importPublish = booleanOrDefault("import_publish", default = false)

      writeService.publishArticle(long("id"), importPublish) match {
        case Success(s) => converterService.toApiStatus(s.status)
        case Failure(ex) => errorHandler(ex)
      }
    }

    post("/concept/:id/publish/?") {
      authRole.assertHasPublishPermission()
      writeService.publishConcept(long("id")) match {
        case Success(s) => s.id.map(ContentId)
        case Failure(ex) => errorHandler(ex)
      }
    }

    post("/empty_article") {
      authRole.assertHasWritePermission()
      val externalId = params("externalId")
      val externalSubjectIds = paramAsListOfString("externalSubjectId")
      writeService.newEmptyArticle(externalId, externalSubjectIds) match {
        case Success(id) => ContentId(id)
        case Failure(ex) => errorHandler(ex)
      }
    }

    post("/empty_concept") {
      authRole.assertHasWritePermission()
      val externalId = params("externalId")
      writeService.newEmptyConcept(externalId) match {
        case Success(id) => id
        case Failure(ex) => errorHandler(ex)
      }
    }


  }
}
