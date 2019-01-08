/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.auth.User
import no.ndla.draftapi.model.api.{
  Concept,
  ConceptSearchParams,
  ConceptSearchResult,
  ContentId,
  Error,
  NewConcept,
  UpdatedConcept
}
import no.ndla.draftapi.model.domain.{Language, Sort}
import no.ndla.draftapi.service.search.{ConceptSearchService, SearchConverterService}
import no.ndla.draftapi.service.{ReadService, WriteService}
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.{NotFound, Ok}
import org.scalatra.swagger.{ResponseMessage, Swagger, SwaggerSupport}

import scala.util.{Failure, Success}

trait ConceptController {
  this: ReadService with WriteService with SearchConverterService with ConceptSearchService with User =>
  val conceptController: ConceptController

  class ConceptController(implicit val swagger: Swagger) extends NdlaController with SwaggerSupport {
    protected implicit override val jsonFormats: Formats = DefaultFormats
    protected val applicationDescription = "API for accessing concepts from ndla.no."

    registerModel[Error]()

    val response400 = ResponseMessage(400, "Validation Error", Some("ValidationError"))
    val response403 = ResponseMessage(403, "Access Denied", Some("Error"))
    val response404 = ResponseMessage(404, "Not found", Some("Error"))
    val response500 = ResponseMessage(500, "Unknown error", Some("Error"))

    private val query =
      Param[Option[String]]("query", "Return only concepts with content matching the specified query.")
    private val conceptIds = Param[Option[Seq[Long]]](
      "ids",
      "Return only concepts that have one of the provided ids. To provide multiple ids, separate by comma (,).")
    private val conceptId = Param[Long]("concept_id", "Id of the concept that is to be returned")

    private def scrollSearchOr(orFunction: => Any): Any = {
      val language = paramOrDefault(this.language.paramName, Language.AllLanguages)

      paramOrNone(this.scrollId.paramName) match {
        case Some(scroll) =>
          conceptSearchService.scroll(scroll, language) match {
            case Success(scrollResult) =>
              val responseHeader = scrollResult.scrollId.map(i => this.scrollId.paramName -> i).toMap
              Ok(searchConverterService.asApiConceptSearchResult(scrollResult), headers = responseHeader)
            case Failure(ex) => errorHandler(ex)
          }
        case None => orFunction
      }
    }

    private def search(query: Option[String],
                       sort: Option[Sort.Value],
                       language: String,
                       page: Int,
                       pageSize: Int,
                       idList: List[Long],
                       fallback: Boolean) = {

      val result = query match {
        case Some(q) =>
          conceptSearchService.matchingQuery(
            query = q,
            withIdIn = idList,
            searchLanguage = language,
            page = page,
            pageSize = pageSize,
            sort = sort.getOrElse(Sort.ByRelevanceDesc),
            fallback = fallback
          )

        case None =>
          conceptSearchService.all(
            withIdIn = idList,
            language = language,
            page = page,
            pageSize = pageSize,
            sort = sort.getOrElse(Sort.ByTitleAsc),
            fallback = fallback
          )
      }

      result match {
        case Success(searchResult) =>
          val responseHeader = searchResult.scrollId.map(i => this.scrollId.paramName -> i).toMap
          Ok(searchConverterService.asApiConceptSearchResult(searchResult), headers = responseHeader)
        case Failure(ex) => errorHandler(ex)
      }

    }

    get(
      "/",
      operation(
        apiOperation[ConceptSearchResult]("getAllConcepts")
          summary "Show all concepts"
          description "Shows all concepts. You can search it too."
          parameters (
            asHeaderParam(correlationId),
            asQueryParam(query),
            asQueryParam(conceptIds),
            asQueryParam(language),
            asQueryParam(query),
            asQueryParam(pageNo),
            asQueryParam(pageSize),
            asQueryParam(sort),
            asQueryParam(fallback),
            asQueryParam(scrollId)
        )
          authorizations "oauth2"
          responseMessages response500)
    ) {
      scrollSearchOr {
        val query = paramOrNone(this.query.paramName)
        val sort = Sort.valueOf(paramOrDefault(this.sort.paramName, ""))
        val language = paramOrDefault(this.language.paramName, Language.NoLanguage)
        val pageSize = intOrDefault(this.pageSize.paramName, DraftApiProperties.DefaultPageSize)
        val page = intOrDefault(this.pageNo.paramName, 1)
        val idList = paramAsListOfLong(this.conceptIds.paramName)
        val fallback = booleanOrDefault(this.fallback.paramName, default = false)

        search(query, sort, language, page, pageSize, idList, fallback)

      }
    }

    post(
      "/search/",
      operation(
        apiOperation[ConceptSearchResult]("searchConcepts")
          summary "Show all concepts"
          description "Shows all concepts. You can search it too."
          parameters (
            asHeaderParam(correlationId),
            bodyParam[ConceptSearchParams],
            asQueryParam(fallback),
            asQueryParam(scrollId)
        )
          authorizations "oauth2"
          responseMessages (response400, response500))
    ) {
      scrollSearchOr {
        extract[ConceptSearchParams](request.body) match {
          case Success(searchParams) =>
            val query = searchParams.query
            val sort = Sort.valueOf(searchParams.sort.getOrElse(""))
            val language = searchParams.language.getOrElse(Language.NoLanguage)
            val pageSize = searchParams.pageSize.getOrElse(DraftApiProperties.DefaultPageSize)
            val page = searchParams.page.getOrElse(1)
            val idList = searchParams.idList
            val fallback = booleanOrDefault(this.fallback.paramName, default = false)

            search(query, sort, language, page, pageSize, idList, fallback)
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    get(
      "/:concept_id",
      operation(
        apiOperation[String]("getConceptById")
          summary "Show concept with a specified id"
          description "Shows the concept for the specified id."
          parameters (
            asHeaderParam(correlationId),
            asQueryParam(language),
            asPathParam(conceptId)
        )
          authorizations "oauth2"
          responseMessages (response404, response500))
    ) {
      val conceptId = long(this.conceptId.paramName)
      val language = paramOrDefault(this.language.paramName, Language.NoLanguage)

      readService.conceptWithId(conceptId, language) match {
        case Some(concept) => concept
        case None          => NotFound(body = Error(Error.NOT_FOUND, s"No concept with id $conceptId found"))
      }
    }

    post(
      "/",
      operation(
        apiOperation[Concept]("newConceptById")
          summary "Create new concept"
          description "Create new concept"
          parameters (
            asHeaderParam(correlationId),
            bodyParam[NewConcept]
        )
          authorizations "oauth2"
          responseMessages (response404, response500))
    ) {
      doOrAccessDenied(user.getUser.canWrite) {
        val nid = params("externalId")
        extract[NewConcept](request.body).flatMap(writeService.newConcept(_, nid)) match {
          case Success(c)  => c
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    patch(
      "/:concept_id",
      operation(
        apiOperation[Concept]("updateConceptById")
          summary "Update a concept"
          description "Update a concept"
          parameters (
            asHeaderParam(correlationId),
            bodyParam[NewConcept]
        )
          authorizations "oauth2"
          responseMessages (response404, response500))
    ) {
      doOrAccessDenied(user.getUser.canWrite) {
        val externalId = paramOrNone("externalId")

        extract[UpdatedConcept](request.body)
          .flatMap(writeService.updateConcept(long(this.conceptId.paramName), _, externalId)) match {
          case Success(c)  => c
          case Failure(ex) => errorHandler(ex)
        }
      }
    }

    get(
      "/external_id/:deprecated_node_id",
      operation(
        apiOperation[ContentId]("getInternalIdByExternalId")
          summary "Get internal id of concept for a specified ndla_node_id"
          description "Get internal id of concept for a specified ndla_node_id"
          parameters (
            asHeaderParam(correlationId),
            asPathParam(deprecatedNodeId)
        )
          authorizations "oauth2"
          responseMessages (response404, response500))
    ) {
      val externalId = long(deprecatedNodeId.paramName)
      readService.getInternalConceptIdByExternalId(externalId) match {
        case Some(id) => id
        case None     => NotFound(body = Error(Error.NOT_FOUND, s"No concept with external id $externalId"))
      }
    }

  }
}
