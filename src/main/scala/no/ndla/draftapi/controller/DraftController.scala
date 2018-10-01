/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.auth.User
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.{api, domain}
import no.ndla.draftapi.model.domain.{ArticleType, Language, Sort}
import no.ndla.draftapi.service.search.ArticleSearchService
import no.ndla.draftapi.service.{ConverterService, ReadService, WriteService}
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.mapping
import no.ndla.mapping.LicenseDefinition
import org.joda.time.DateTime
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.swagger.{ResponseMessage, Swagger}
import org.scalatra.{Created, NoContent, NotFound, Ok}

import scala.util.{Failure, Success}

trait DraftController {
  this: ReadService with WriteService with ArticleSearchService with ConverterService with ContentValidator with User =>
  val draftController: DraftController

  class DraftController(implicit val swagger: Swagger) extends NdlaController {
    protected implicit override val jsonFormats: Formats = DefaultFormats
    protected val applicationDescription = "API for accessing draft articles from ndla.no."

    // Additional models used in error responses
    registerModel[ValidationError]()
    registerModel[Error]()

    val converterService = new ConverterService
    val response400 = ResponseMessage(400, "Validation Error", Some("ValidationError"))
    val response403 = ResponseMessage(403, "Access Denied", Some("Error"))
    val response404 = ResponseMessage(404, "Not found", Some("Error"))
    val response500 = ResponseMessage(500, "Unknown error", Some("Error"))

    private val query = Param("query", "Return only articles with content matching the specified query.")
    private val articleId = Param("article_id", "Id of the article that is to be fecthed")
    private val size = Param("size", "Limit the number of results to this many elements")
    private val articleTypes = Param(
      "articleTypes",
      "Return only articles of specific type(s). To provide multiple types, separate by comma (,).")
    private val articleIds = Param(
      "ids",
      "Return only articles that have one of the provided ids. To provide multiple ids, separate by comma (,).")
    private val filter = Param("filter", "A filter to include a specific entry")
    private val filterNot = Param("filterNot", "A filter to remove a specific entry")
    private val statuss = Param("STATUS", "An article status")

    val getTags =
      (apiOperation[ArticleTag]("getTags")
        summary "Retrieves a list of all previously used tags in articles"
        notes "Retrieves a list of all previously used tags in articles"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asQueryParam[Option[Int]](size),
          asQueryParam[Option[String]](language)
      )
        responseMessages response500
        authorizations "oauth2")

    get("/tags/", operation(getTags)) {
      val defaultSize = 20
      val language = paramOrDefault("language", Language.AllLanguages)
      val size = intOrDefault("size", defaultSize) match {
        case tooSmall if tooSmall < 1 => defaultSize
        case x                        => x
      }
      val tags = readService.getNMostUsedTags(size, language)
      if (tags.isEmpty) {
        NotFound(body = Error(Error.NOT_FOUND, s"No tags with language $language was found"))
      } else {
        tags
      }
    }

    private def search(query: Option[String],
                       sort: Option[Sort.Value],
                       language: String,
                       license: Option[String],
                       page: Int,
                       pageSize: Int,
                       idList: List[Long],
                       articleTypesFilter: Seq[String],
                       fallback: Boolean) = {
      val result = query match {
        case Some(q) =>
          articleSearchService.matchingQuery(
            query = q,
            withIdIn = idList,
            searchLanguage = language,
            license = license,
            page = page,
            pageSize = if (idList.isEmpty) pageSize else idList.size,
            sort = sort.getOrElse(Sort.ByRelevanceDesc),
            if (articleTypesFilter.isEmpty) ArticleType.all else articleTypesFilter,
            fallback = fallback
          )
        case None =>
          articleSearchService.all(
            withIdIn = idList,
            language = language,
            license = license,
            page = page,
            pageSize = if (idList.isEmpty) pageSize else idList.size,
            sort = sort.getOrElse(Sort.ByTitleAsc),
            if (articleTypesFilter.isEmpty) ArticleType.all else articleTypesFilter,
            fallback = fallback
          )
      }

      result match {
        case Success(searchResult) => searchResult
        case Failure(ex)           => errorHandler(ex)
      }
    }

    val getAllArticles =
      (apiOperation[List[SearchResult]]("getAllArticles")
        summary "Show all articles"
        notes "Shows all articles. You can search it too."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asQueryParam[Option[String]](articleTypes),
          asQueryParam[Option[String]](query),
          asQueryParam[Option[String]](articleIds),
          asQueryParam[Option[String]](language),
          asQueryParam[Option[String]](license),
          asQueryParam[Option[Int]](pageNo),
          asQueryParam[Option[Int]](pageSize),
          asQueryParam[Option[String]](sort)
      )
        authorizations "oauth2"
        responseMessages (response500))

    get("/", operation(getAllArticles)) {
      val query = paramOrNone(this.query.paramName)
      val sort = Sort.valueOf(paramOrDefault(this.sort.paramName, ""))
      val language = paramOrDefault(this.language.paramName, Language.AllLanguages)
      val license = paramOrNone(this.license.paramName)
      val pageSize = intOrDefault(this.pageSize.paramName, DraftApiProperties.DefaultPageSize)
      val page = intOrDefault(this.pageNo.paramName, 1)
      val idList = paramAsListOfLong(this.articleIds.paramName)
      val articleTypesFilter = paramAsListOfString(this.articleTypes.paramName)
      val fallback = booleanOrDefault(this.fallback.paramName, default = false)

      search(query, sort, language, license, page, pageSize, idList, articleTypesFilter, fallback)
    }

    val getAllArticlesPost =
      (apiOperation[List[SearchResult]]("getAllArticlesPost")
        summary "Show all articles"
        notes "Shows all articles. You can search it too."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asQueryParam[Option[String]](language),
          bodyParam[ArticleSearchParams]
      )
        authorizations "oauth2"
        responseMessages (response400, response500))

    post("/search/", operation(getAllArticlesPost)) {
      val searchParams = extract[ArticleSearchParams](request.body)

      val query = searchParams.query
      val sort = Sort.valueOf(searchParams.sort.getOrElse(""))
      val language = searchParams.language.getOrElse(Language.AllLanguages)
      val license = searchParams.license
      val pageSize = searchParams.pageSize.getOrElse(DraftApiProperties.DefaultPageSize)
      val page = searchParams.page.getOrElse(1)
      val idList = searchParams.idList
      val articleTypesFilter = searchParams.articleTypes
      val fallback = booleanOrDefault(this.fallback.paramName, default = false)

      search(query, sort, language, license, page, pageSize, idList, articleTypesFilter, fallback)
    }

    val getArticleById =
      (apiOperation[Article]("getArticleById")
        summary "Show article with a specified Id"
        notes "Shows the article for the specified id."
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asPathParam[Long](articleId),
          asQueryParam[Option[String]](language),
          asQueryParam[Option[Boolean]](fallback)
      )
        authorizations "oauth2"
        responseMessages (response404, response500))

    get("/:article_id", operation(getArticleById)) {
      val articleId = long(this.articleId.paramName)
      val language = paramOrDefault(this.language.paramName, Language.AllLanguages)
      val fallback = booleanOrDefault(this.fallback.paramName, false)

      readService.withId(articleId, language, fallback) match {
        case Success(article) => article
        case Failure(ex)      => errorHandler(ex)
      }
    }

    val getInternalIdByExternalId =
      (apiOperation[ContentId]("getInternalIdByExternalId")
        summary "Get internal id of article for a specified ndla_node_id"
        notes "Get internal id of article for a specified ndla_node_id"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asPathParam[Long](deprecatedNodeId)
      )
        authorizations "oauth2"
        responseMessages (response404, response500))

    get("/external_id/:deprecated_node_id", operation(getInternalIdByExternalId)) {
      val externalId = long(this.deprecatedNodeId.paramName)
      readService.getInternalArticleIdByExternalId(externalId) match {
        case Some(id) => id
        case None     => NotFound(body = Error(Error.NOT_FOUND, s"No article with id $externalId"))
      }
    }

    val getLicenses =
      (apiOperation[List[License]]("getLicenses")
        summary "Show all valid licenses"
        notes "Shows all valid licenses"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asQueryParam[Option[String]](filter),
          asQueryParam[Option[String]](filterNot)
      )
        responseMessages (response403, response500)
        authorizations "oauth2")

    get("/licenses/", operation(getLicenses)) {
      val filterNot = paramOrNone(this.filterNot.paramName)
      val filter = paramOrNone(this.filter.paramName)

      val licenses: Seq[LicenseDefinition] = mapping.License.getLicenses
        .filter {
          case license: LicenseDefinition if filter.isDefined => license.license.toString.contains(filter.get)
          case _                                              => true
        }
        .filterNot {
          case license: LicenseDefinition if filterNot.isDefined => license.license.toString.contains(filterNot.get)
          case _                                                 => false
        }

      licenses.map(x => License(x.license.toString, Option(x.description), x.url))
    }

    val newArticle =
      (apiOperation[Article]("newArticle")
        summary "Create a new article"
        notes "Creates a new article"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          bodyParam[NewArticle]
      )
        authorizations "oauth2"
        responseMessages (response400, response403, response500))

    post("/", operation(newArticle)) {
      val userInfo = user.getUser
      doOrAccessDenied(userInfo.canWrite) {
        val externalId = paramAsListOfString("externalId")
        val oldNdlaCreatedDate = paramOrNone("oldNdlaCreatedDate").map(new DateTime(_).toDate)
        val oldNdlaUpdatedDate = paramOrNone("oldNdlaUpdatedDate").map(new DateTime(_).toDate)
        val externalSubjectids = paramAsListOfString("externalSubjectIds")
        writeService.newArticle(extract[NewArticle](request.body),
                                externalId,
                                externalSubjectids,
                                userInfo,
                                oldNdlaCreatedDate,
                                oldNdlaUpdatedDate) match {
          case Success(article)   => Created(body = article)
          case Failure(exception) => errorHandler(exception)
        }
      }
    }

    val updateArticle =
      (apiOperation[Article]("updateArticle")
        summary "Update an existing article"
        notes "Update an existing article"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asPathParam[Long](articleId),
          bodyParam[UpdatedArticle]
      )
        authorizations "oauth2"
        responseMessages (response400, response403, response404, response500))

    patch("/:article_id", operation(updateArticle)) {
      val userInfo = user.getUser
      doOrAccessDenied(userInfo.canWrite) {
        val externalId = paramAsListOfString("externalId")
        val externalSubjectIds = paramAsListOfString("externalSubjectIds")
        val oldNdlaCreateddDate = paramOrNone("oldNdlaCreatedDate").map(new DateTime(_).toDate)
        val oldNdlaUpdatedDate = paramOrNone("oldNdlaUpdatedDate").map(new DateTime(_).toDate)
        val id = long(this.articleId.paramName)
        val updateArticle = extract[UpdatedArticle](request.body)

        writeService.updateArticle(id,
                                   updateArticle,
                                   externalId,
                                   externalSubjectIds,
                                   userInfo,
                                   oldNdlaCreateddDate,
                                   oldNdlaUpdatedDate) match {
          case Success(article)   => Ok(body = article)
          case Failure(exception) => errorHandler(exception)
        }
      }
    }

    put(
      "/:article_id/status/:STATUS",
      operation(
        apiOperation[Article]("updateArticleStatus")
          summary "Update status of an article"
          notes "Update status of an article"
          parameters (
            asPathParam[Long](articleId),
            asPathParam[String](statuss)
        )
          authorizations "oauth2"
          responseMessages (response400, response403, response404, response500))
    ) {
      val userInfo = user.getUser
      doOrAccessDenied(userInfo.canWrite) {
        val id = long(this.articleId.paramName)
        domain.ArticleStatus
          .valueOfOrError(params(this.statuss.paramName))
          .flatMap(writeService.updateArticleStatus(_, id, userInfo)) match {
          case Success(a)  => a
          case Failure(ex) => errorHandler(ex)
        }

      }
    }

    val validateArticle =
      (apiOperation[ContentId]("validateArticle")
        summary "Validate an article"
        notes "Validate an article"
        parameters (
          asHeaderParam[Option[String]](correlationId),
          asPathParam[Long](articleId)
      )
        authorizations "oauth2"
        responseMessages (response400, response403, response404, response500))

    put("/:article_id/validate/", operation(validateArticle)) {
      contentValidator.validateArticleApiArticle(long(this.articleId.paramName)) match {
        case Success(id) => id
        case Failure(ex) => errorHandler(ex)
      }
    }

    get(
      "/status-state-machine/",
      operation(
        apiOperation[Map[String, List[String]]]("getStatusStateMachine")
          summary "Get status state machine"
          notes "Get status state machine"
          authorizations "oauth2"
          responseMessages response500
      )
    ) {
      converterService.stateTransitionsToApi(user.getUser)
    }

  }
}
