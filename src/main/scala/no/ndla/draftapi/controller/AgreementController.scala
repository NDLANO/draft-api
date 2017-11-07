/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.controller

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.DraftApiProperties.RoleWithWriteAccess
import no.ndla.draftapi.auth.Role
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.domain.{ArticleType, Language, Sort}
import no.ndla.draftapi.service.search.ArticleSearchService
import no.ndla.draftapi.service.{ConverterService, ReadService, WriteService}
import no.ndla.mapping
import no.ndla.mapping.LicenseDefinition
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.swagger.{ResponseMessage, Swagger, SwaggerSupport}
import org.scalatra.{Created, NotFound, Ok}

import scala.util.{Failure, Success}

trait AgreementController {
  this: ReadService with WriteService with ArticleSearchService with ConverterService with Role =>
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

    val getAllAgreements =
      (apiOperation[AgreementSearchResult]("getAllAgreements")
        summary "Show all agreements"
        notes "Shows all agreements. You can search too."
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id. May be omitted."),
        queryParam[Option[String]]("articleTypes").description("Return only articles of specific type(s). To provide multiple types, separate by comma (,)."),
        queryParam[Option[String]]("query").description("Return only articles with content matching the specified query."),
        queryParam[Option[String]]("ids").description("Return only articles that have one of the provided ids. To provide multiple ids, separate by comma (,)."),
        queryParam[Option[String]]("language").description("Only return results on the given language. Default is nb"),
        queryParam[Option[String]]("license").description("Return only articles with provided license."),
        queryParam[Option[Int]]("page").description("The page number of the search hits to display."),
        queryParam[Option[Int]]("page-size").description("The number of search hits to display for each page."),
        queryParam[Option[String]]("sort").description(
          """The sorting used on results.
             Default is by -relevance (desc) when querying.
             When browsing, the default is title (asc).
             The following are supported: relevance, -relevance, title, -title, lastUpdated, -lastUpdated, id, -id""".stripMargin)
      )
        authorizations "oauth2"
        responseMessages(response500))

    get("/", operation(getAllAgreements)) {
      val query = paramOrNone("query")
      val sort = Sort.valueOf(paramOrDefault("sort", ""))
      val language = paramOrDefault("language", Language.DefaultLanguage)
      val license = paramOrNone("license")
      val pageSize = intOrDefault("page-size", DraftApiProperties.DefaultPageSize)
      val page = intOrDefault("page", 1)
      val idList = paramAsListOfLong("ids")
      val articleTypesFilter = paramAsListOfString("articleTypes")
    }

  }
}
