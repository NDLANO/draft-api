/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.integration.ReindexClient
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.domain.Sort
import no.ndla.draftapi.service.search.{AgreementSearchService, SearchConverterService}
import no.ndla.draftapi.service.{ConverterService, ReadService, WriteService}
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.swagger.{ResponseMessage, Swagger, SwaggerSupport}
import org.scalatra.{Created, NotFound, Ok}

import scala.util.{Failure, Success}

trait AgreementController {
  this: ReadService
    with WriteService
    with AgreementSearchService
    with ConverterService
    with SearchConverterService
    with ReindexClient =>
  val agreementController: AgreementController

  class AgreementController(implicit val swagger: Swagger) extends NdlaController with SwaggerSupport {
    protected implicit override val jsonFormats: Formats = DefaultFormats
    protected val applicationDescription = "API for accessing agreements from ndla.no."

    // Additional models used in error responses
    registerModel[ValidationError]()
    registerModel[Error]()

    val converterService = new ConverterService
    val response400 = ResponseMessage(400, "Validation Error", Some("ValidationError"))
    val response403 = ResponseMessage(403, "Access Denied", Some("Error"))
    val response404 = ResponseMessage(404, "Not found", Some("Error"))
    val response500 = ResponseMessage(500, "Unknown error", Some("Error"))

    private val query = Param("query", "Return only agreements with content matching the specified query.")
    private val agreementIds = Param(
      "ids",
      "Return only agreements that have one of the provided ids. To provide multiple ids, separate by comma (,).")
    private val agreementId = Param("agreement_id", "Id of the agreement that is to be returned")

    private def search(query: Option[String],
                       sort: Option[Sort.Value],
                       license: Option[String],
                       page: Int,
                       pageSize: Int,
                       idList: List[Long]) = {
      val result = query match {
        case Some(q) =>
          agreementSearchService.matchingQuery(
            query = q,
            withIdIn = idList,
            license = license,
            page = page,
            pageSize = if (idList.isEmpty) pageSize else idList.size,
            sort = sort.getOrElse(Sort.ByTitleAsc)
          )
        case None =>
          agreementSearchService.all(
            withIdIn = idList,
            license = license,
            page = page,
            pageSize = if (idList.isEmpty) pageSize else idList.size,
            sort = sort.getOrElse(Sort.ByTitleAsc)
          )
      }

      result match {
        case Success(searchResult) => searchResult
        case Failure(ex)           => errorHandler(ex)
      }
    }

    val getAllAgreements =
      (apiOperation[AgreementSearchResult]("getAllAgreements")
        summary "Show all agreements"
        notes "Shows all agreements. You can search too."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asQueryParam[Option[String]](query),
          asQueryParam[Option[String]](agreementIds),
          asQueryParam[Option[String]](language),
          asQueryParam[Option[String]](license),
          asQueryParam[Option[Int]](pageNo),
          asQueryParam[Option[Int]](pageSize),
          asQueryParam[Option[String]](sort)
      )
        authorizations "oauth2"
        responseMessages response500)

    get("/", operation(getAllAgreements)) {
      val query = paramOrNone(this.query.paramName)
      val sort = Sort.valueOf(paramOrDefault(this.sort.paramName, ""))
      val license = paramOrNone(this.license.paramName)
      val pageSize = intOrDefault(this.pageSize.paramName, DraftApiProperties.DefaultPageSize)
      val page = intOrDefault(this.pageNo.paramName, 1)
      val idList = paramAsListOfLong(this.agreementIds.paramName)

      search(query, sort, license, page, pageSize, idList)
    }

    val getAgreementById =
      (apiOperation[Agreement]("getAgreementById")
        summary "Show agreement with a specified Id"
        notes "Shows the agreement for the specified id."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          pathParam[Long]("agreement_id").description("Id of the article that is to be returned")
      )
        authorizations "oauth2"
        responseMessages (response404, response500))

    get("/:agreement_id", operation(getAgreementById)) {
      val agreementId = long(this.agreementId.paramName)

      readService.agreementWithId(agreementId) match {
        case Some(agreement) => agreement
        case None            => NotFound(body = Error(Error.NOT_FOUND, s"No agreement with id $agreementId found"))
      }
    }

    val newAgreement =
      (apiOperation[Agreement]("newAgreement")
        summary "Create a new agreement"
        notes "Creates a new agreement"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          bodyParam[NewAgreement]
      )
        authorizations "oauth2"
        responseMessages (response400, response403, response500))

    post("/", operation(newAgreement)) {
      val user = getUser
      doOrAccessDenied(user.canWrite) {
        val newAgreement = extract[NewAgreement](request.body)
        writeService.newAgreement(newAgreement, user) match {
          case Success(agreement) =>
            reindexClient.reindexAll()
            Created(body = agreement)
          case Failure(exception) => errorHandler(exception)
        }
      }
    }

    val updateAgreement =
      (apiOperation[Agreement]("updateAgreement")
        summary "Update an existing agreement"
        notes "Update an existing agreement"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asPathParam[Long](agreementId),
          bodyParam[UpdatedAgreement]
      )
        authorizations "oauth2"
        responseMessages (response400, response403, response404, response500))

    patch("/:agreement_id", operation(updateAgreement)) {
      val user = getUser

      doOrAccessDenied(user.canWrite) {
        val agreementId = long(this.agreementId.paramName)
        val updatedAgreement = extract[UpdatedAgreement](request.body)
        writeService.updateAgreement(agreementId, updatedAgreement, user) match {
          case Success(agreement) =>
            reindexClient.reindexAll()
            Ok(body = agreement)
          case Failure(exception) => errorHandler(exception)
        }
      }
    }

  }
}
