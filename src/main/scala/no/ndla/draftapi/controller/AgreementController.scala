/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.controller

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.auth.{Role, User}
import no.ndla.draftapi.integration.ReindexClient
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.domain.{Language, Sort}
import no.ndla.draftapi.service.search.{AgreementSearchService, SearchConverterService}
import no.ndla.draftapi.service.{ConverterService, ReadService, WriteService}
import no.ndla.mapping
import no.ndla.mapping.LicenseDefinition
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.swagger.{ResponseMessage, Swagger, SwaggerSupport}
import org.scalatra.{Created, NotFound, Ok}

import scala.util.{Failure, Success}

trait AgreementController {
  this: ReadService with WriteService with AgreementSearchService with ConverterService with SearchConverterService with ReindexClient with Role with User =>
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

    private def search(query: Option[String], sort: Option[Sort.Value], license: Option[String], page: Int, pageSize: Int, idList: List[Long]) = {
      query match {
        case Some(q) => agreementSearchService.matchingQuery(
          query = q,
          withIdIn = idList,
          license = license,
          page = page,
          pageSize = if (idList.isEmpty) pageSize else idList.size,
          sort = sort.getOrElse(Sort.ByTitleAsc)
        )
        case None => agreementSearchService.all(
          withIdIn = idList,
          license = license,
          page = page,
          pageSize = if (idList.isEmpty) pageSize else idList.size,
          sort = sort.getOrElse(Sort.ByTitleAsc)
        )
      }
    }

    val getAllAgreements =
      (apiOperation[AgreementSearchResult]("getAllAgreements")
        summary "Show all agreements"
        notes "Shows all agreements. You can search too."
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id. May be omitted."),
        queryParam[Option[String]]("query").description("Return only articles with content matching the specified query."),
        queryParam[Option[String]]("ids").description("Return only agreements that have one of the provided ids. To provide multiple ids, separate by comma (,)."),
        queryParam[Option[String]]("language").description("Only return results on the given language. Default is nb"),
        queryParam[Option[String]]("license").description("Return only agreements with provided license."),
        queryParam[Option[Int]]("page").description("The page number of the search hits to display."),
        queryParam[Option[Int]]("page-size").description("The number of search hits to display for each page."),
        queryParam[Option[String]]("sort").description(
          """The sorting used on results.
             Default is by -relevance (desc) when querying.
             When browsing, the default is title (asc).
             The following are supported: relevance, -relevance, title, -title, lastUpdated, -lastUpdated, id, -id""".stripMargin)
      )
        authorizations "oauth2"
        responseMessages response500)

    get("/", operation(getAllAgreements)) {
      val query = paramOrNone("query")
      val sort = Sort.valueOf(paramOrDefault("sort", ""))
      val license = paramOrNone("license")
      val pageSize = intOrDefault("page-size", DraftApiProperties.DefaultPageSize)
      val page = intOrDefault("page", 1)
      val idList = paramAsListOfLong("ids")

      search(query, sort, license, page, pageSize, idList)
    }

    val getAgreementById =
      (apiOperation[Agreement]("getAgreementById")
        summary "Show agreement with a specified Id"
        notes "Shows the agreement for the specified id."
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id. May be omitted."),
        pathParam[Long]("agreement_id").description("Id of the article that is to be returned")
      )
        authorizations "oauth2"
        responseMessages(response404, response500))

    get("/:agreement_id", operation(getAgreementById)) {
      val agreementId = long("agreement_id")

      readService.agreementWithId(agreementId) match {
        case Some(agreement) => agreement
        case None => NotFound(body = Error(Error.NOT_FOUND, s"No agreement with id $agreementId found"))
      }
    }

    val newAgreement =
      (apiOperation[Agreement]("newAgreement")
        summary "Create a new agreement"
        notes "Creates a new agreement"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id"),
        bodyParam[NewAgreement]
      )
        authorizations "oauth2"
        responseMessages(response400, response403, response500))

    post("/", operation(newAgreement)) {
      authUser.assertHasId()
      authRole.assertHasWritePermission()
      val newAgreement = extract[NewAgreement](request.body)
      writeService.newAgreement(newAgreement) match {
        case Success(agreement) =>
          reindexClient.reindexAll()
          Created(body=agreement)
        case Failure(exception) => errorHandler(exception)
      }
    }

    val updateAgreement =
      (apiOperation[Agreement]("updateAgreement")
        summary "Update an existing agreement"
        notes "Update an existing agreement"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id"),
        pathParam[Long]("article_id").description("Id of the agreement that is to be updated"),
        bodyParam[UpdatedAgreement]
      )
        authorizations "oauth2"
        responseMessages(response400, response403, response404, response500))

    patch("/:agreement_id", operation(updateAgreement)) {
      authUser.assertHasId()
      authRole.assertHasWritePermission()

      val agreementId = long("agreement_id")
      val updatedAgreement = extract[UpdatedAgreement](request.body)
      writeService.updateAgreement(agreementId, updatedAgreement) match {
        case Success(agreement) =>
          reindexClient.reindexAll()
          Ok(body=agreement)
        case Failure(exception) => errorHandler(exception)
      }
    }

  }
}
