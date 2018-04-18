/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.auth.Role
import no.ndla.draftapi.model.api.{
  ContentId,
  Concept,
  ConceptSearchParams,
  ConceptSearchResult,
  Error,
  NewConcept,
  UpdatedConcept
}
import no.ndla.draftapi.model.domain.{Language, Sort}
import no.ndla.draftapi.service.{ReadService, WriteService}
import no.ndla.draftapi.service.search.ConceptSearchService
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.NotFound
import org.scalatra.swagger.{ResponseMessage, Swagger, SwaggerSupport}

import scala.util.{Failure, Success}

trait ConceptController {
  this: ReadService with WriteService with ConceptSearchService with Role =>
  val conceptController: ConceptController

  class ConceptController(implicit val swagger: Swagger) extends NdlaController with SwaggerSupport {
    protected implicit override val jsonFormats: Formats = DefaultFormats
    protected val applicationDescription = "API for accessing concepts from ndla.no."

    registerModel[Error]()

    val response400 = ResponseMessage(400, "Validation Error", Some("ValidationError"))
    val response403 = ResponseMessage(403, "Access Denied", Some("Error"))
    val response404 = ResponseMessage(404, "Not found", Some("Error"))
    val response500 = ResponseMessage(500, "Unknown error", Some("Error"))

    private val query = Param("query", "Return only concepts with content matching the specified query.")
    private val conceptIds = Param(
      "ids",
      "Return only concepts that have one of the provided ids. To provide multiple ids, separate by comma (,).")
    private val conceptId = Param("concept_id", "Id of the concept that is to be returned")

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
        case Success(searchResult) => searchResult
        case Failure(ex)           => errorHandler(ex)
      }

    }

    val getAllConcepts =
      (apiOperation[ConceptSearchResult]("getAllConcepts")
        summary "Show all concepts"
        notes "Shows all concepts. You can search it too."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asQueryParam[Option[String]](query),
          asQueryParam[Option[String]](conceptIds),
          asQueryParam[Option[String]](language),
          asQueryParam[Option[String]](query),
          asQueryParam[Option[Int]](pageNo),
          asQueryParam[Option[Int]](pageSize),
          asQueryParam[Option[String]](sort),
          asQueryParam[Option[Boolean]](fallback)
      )
        authorizations "oauth2"
        responseMessages (response500))

    get("/", operation(getAllConcepts)) {
      val query = paramOrNone(this.query.paramName)
      val sort = Sort.valueOf(paramOrDefault(this.sort.paramName, ""))
      val language = paramOrDefault(this.language.paramName, Language.NoLanguage)
      val pageSize = intOrDefault(this.pageSize.paramName, DraftApiProperties.DefaultPageSize)
      val page = intOrDefault(this.pageNo.paramName, 1)
      val idList = paramAsListOfLong(this.conceptIds.paramName)
      val fallback = booleanOrDefault(this.fallback.paramName, default = false)

      search(query, sort, language, page, pageSize, idList, fallback)
    }

    val getAllConceptsPost =
      (apiOperation[ConceptSearchResult]("searchConcepts")
        summary "Show all concepts"
        notes "Shows all concepts. You can search it too."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          bodyParam[ConceptSearchParams],
          asQueryParam[Option[Boolean]](fallback)
      )
        authorizations "oauth2"
        responseMessages (response400, response500))

    post("/search/", operation(getAllConceptsPost)) {
      val searchParams = extract[ConceptSearchParams](request.body)

      val query = searchParams.query
      val sort = Sort.valueOf(searchParams.sort.getOrElse(""))
      val language = searchParams.language.getOrElse(Language.NoLanguage)
      val pageSize = searchParams.pageSize.getOrElse(DraftApiProperties.DefaultPageSize)
      val page = searchParams.page.getOrElse(1)
      val idList = searchParams.idList
      val fallback = booleanOrDefault(this.fallback.paramName, default = false)

      search(query, sort, language, page, pageSize, idList, fallback)
    }

    val getConceptById =
      (apiOperation[String]("getConceptById")
        summary "Show concept with a specified id"
        notes "Shows the concept for the specified id."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asQueryParam[Option[String]](language),
          asPathParam[Long](conceptId)
      )
        authorizations "oauth2"
        responseMessages (response404, response500))

    get("/:concept_id", operation(getConceptById)) {
      val conceptId = long(this.conceptId.paramName)
      val language = paramOrDefault(this.language.paramName, Language.NoLanguage)

      readService.conceptWithId(conceptId, language) match {
        case Some(concept) => concept
        case None          => NotFound(body = Error(Error.NOT_FOUND, s"No concept with id $conceptId found"))
      }
    }

    val newConcept =
      (apiOperation[Concept]("newConceptById")
        summary "Create new concept"
        notes "Create new concept"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          bodyParam[NewConcept]
      )
        authorizations "oauth2"
        responseMessages (response404, response500))

    post("/", operation(newConcept)) {
      authRole.assertHasWritePermission()
      val nid = params("externalId")
      writeService.newConcept(extract[NewConcept](request.body), nid) match {
        case Success(c)  => c
        case Failure(ex) => errorHandler(ex)
      }
    }

    val updateConcept =
      (apiOperation[Concept]("updateConceptById")
        summary "Update a concept"
        notes "Update a concept"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          bodyParam[NewConcept]
      )
        authorizations "oauth2"
        responseMessages (response404, response500))

    patch("/:concept_id", operation(updateConcept)) {
      authRole.assertHasWritePermission()
      val externalId = paramOrNone("externalId")

      writeService.updateConcept(long(this.conceptId.paramName), extract[UpdatedConcept](request.body), externalId) match {
        case Success(c)  => c
        case Failure(ex) => errorHandler(ex)
      }
    }

    val getInternalIdByExternalId =
      (apiOperation[ContentId]("getInternalIdByExternalId")
        summary "Get internal id of concept for a specified ndla_node_id"
        notes "Get internal id of concept for a specified ndla_node_id"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asPathParam[Long](deprecatedNodeId)
      )
        authorizations "oauth2"
        responseMessages (response404, response500))

    get("/external_id/:deprecated_node_id", operation(getInternalIdByExternalId)) {
      val externalId = long(deprecatedNodeId.paramName)
      readService.getInternalConceptIdByExternalId(externalId) match {
        case Some(id) => id
        case None     => NotFound(body = Error(Error.NOT_FOUND, s"No concept with external id $externalId"))
      }
    }

  }
}
