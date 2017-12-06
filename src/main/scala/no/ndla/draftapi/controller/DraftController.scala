/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.controller

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.auth.{Role, User}
import no.ndla.draftapi.model.api._
import no.ndla.draftapi.model.domain.{ArticleType, Language, Sort}
import no.ndla.draftapi.service.search.ArticleSearchService
import no.ndla.draftapi.service.{ConverterService, ReadService, WriteService}
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.mapping
import no.ndla.mapping.LicenseDefinition
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.swagger.{ResponseMessage, Swagger, SwaggerSupport}
import org.scalatra.{Created, NoContent, NotFound, Ok}

import scala.util.{Failure, Success}

trait DraftController {
  this: ReadService with WriteService with ArticleSearchService with ConverterService with Role with User with ContentValidator =>
  val draftController: DraftController

  class DraftController(implicit val swagger: Swagger) extends NdlaController with SwaggerSupport {
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


    val getTags =
      (apiOperation[ArticleTag]("getTags")
        summary "Retrieves a list of all previously used tags in articles"
        notes "Retrieves a list of all previously used tags in articles"
        parameters(
          queryParam[Option[Int]]("size").description("Limit the number of results to this many elements"),
          headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id. May be omitted."),
          queryParam[Option[String]]("language").description("Only return results on the given language. Default is nb")
        )
        responseMessages response500
        authorizations "oauth2")

    get("/tags/?", operation(getTags)) {
      val defaultSize = 20
      val language = paramOrDefault("language", Language.AllLanguages)
      val size = intOrDefault("size", defaultSize) match {
        case tooSmall if tooSmall < 1 => defaultSize
        case x => x
      }
      val tags = readService.getNMostUsedTags(size, language)
      if (tags.isEmpty) {
        NotFound(body = Error(Error.NOT_FOUND, s"No tags with language $language was found"))
      } else {
        tags
      }
    }

    private def search(query: Option[String], sort: Option[Sort.Value], language: String, license: Option[String], page: Int, pageSize: Int, idList: List[Long], articleTypesFilter: Seq[String]) = {
      query match {
        case Some(q) =>
          articleSearchService.matchingQuery(
            query = q,
            withIdIn = idList,
            searchLanguage = language,
            license = license,
            page = page,
            pageSize = if (idList.isEmpty) pageSize else idList.size,
            sort = sort.getOrElse(Sort.ByRelevanceDesc),
            if (articleTypesFilter.isEmpty) ArticleType.all else articleTypesFilter
          )

        case None =>
          articleSearchService.all(
            withIdIn = idList,
            language = language,
            license = license,
            page = page,
            pageSize = if (idList.isEmpty) pageSize else idList.size,
            sort = sort.getOrElse(Sort.ByTitleAsc),
            if (articleTypesFilter.isEmpty) ArticleType.all else articleTypesFilter
          )
      }
    }

    val getAllArticles =
      (apiOperation[List[SearchResult]]("getAllArticles")
        summary "Show all articles"
        notes "Shows all articles. You can search it too."
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

    get("/", operation(getAllArticles)) {
      val query = paramOrNone("query")
      val sort = Sort.valueOf(paramOrDefault("sort", ""))
      val language = paramOrDefault("language", Language.DefaultLanguage)
      val license = paramOrNone("license")
      val pageSize = intOrDefault("page-size", DraftApiProperties.DefaultPageSize)
      val page = intOrDefault("page", 1)
      val idList = paramAsListOfLong("ids")
      val articleTypesFilter = paramAsListOfString("articleTypes")

      search(query, sort, language, license, page, pageSize, idList, articleTypesFilter)
    }

    val getAllArticlesPost =
      (apiOperation[List[SearchResult]]("getAllArticlesPost")
        summary "Show all articles"
        notes "Shows all articles. You can search it too."
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id"),
        queryParam[Option[String]]("language").description("Only return results on the given language. Default is nb"),
        bodyParam[ArticleSearchParams]
      )
        authorizations "oauth2"
        responseMessages(response400, response500))

    post("/search/", operation(getAllArticlesPost)) {
      val searchParams = extract[ArticleSearchParams](request.body)

      val query = searchParams.query
      val sort = Sort.valueOf(searchParams.sort.getOrElse(""))
      val language = searchParams.language.getOrElse(Language.DefaultLanguage)
      val license = searchParams.license
      val pageSize = searchParams.pageSize.getOrElse(DraftApiProperties.DefaultPageSize)
      val page = searchParams.page.getOrElse(1)
      val idList = searchParams.idList
      val articleTypesFilter = searchParams.articleTypes

      search(query, sort, language, license, page, pageSize, idList, articleTypesFilter)
    }

    val getArticleById =
      (apiOperation[List[Article]]("getArticleById")
        summary "Show article with a specified Id"
        notes "Shows the article for the specified id."
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id. May be omitted."),
        pathParam[Long]("article_id").description("Id of the article that is to be returned"),
        queryParam[Option[String]]("language").description("Only return results on the given language. Default is nb")
      )
        authorizations "oauth2"
        responseMessages(response404, response500))

    get("/:article_id", operation(getArticleById)) {
      val articleId = long("article_id")
      val language = paramOrDefault("language", Language.AllLanguages)

      readService.withId(articleId, language) match {
        case Some(article) => article
        case None => NotFound(body = Error(Error.NOT_FOUND, s"No article with id $articleId and language $language found"))
      }
    }

    val getInternalIdByExternalId =
      (apiOperation[ArticleId]("getInternalIdByExternalId")
        summary "Get internal id of article for a specified ndla_node_id"
        notes "Get internal id of article for a specified ndla_node_id"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id. May be omitted."),
        pathParam[Long]("ndla_node_id").description("Id of old NDLA node")
      )
        authorizations "oauth2"
        responseMessages(response404, response500))

    get("/external_id/:ndla_node_id", operation(getInternalIdByExternalId)) {
      val externalId = long("ndla_node_id")
      readService.getInternalIdByExternalId(externalId) match {
        case Some(id) => id
        case None => NotFound(body = Error(Error.NOT_FOUND, s"No article with id $externalId"))
      }
    }

    val getLicenses =
      (apiOperation[List[License]]("getLicenses")
        summary "Show all valid licenses"
        notes "Shows all valid licenses"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id. May be omitted."),
        queryParam[Option[String]]("filter").description("A filter on the license keys. May be omitted"))
        responseMessages(response403, response500)
        authorizations "oauth2")

    get("/licenses", operation(getLicenses)) {
      val licenses: Seq[LicenseDefinition] = paramOrNone("filter") match {
        case None => mapping.License.getLicenses
        case Some(filter) => mapping.License.getLicenses.filter(_.license.contains(filter))
      }

      licenses.map(x => License(x.license, Option(x.description), x.url))
    }

    val newArticle =
      (apiOperation[Article]("newArticle")
        summary "Create a new article"
        notes "Creates a new article"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id"),
        bodyParam[NewArticle]
      )
        authorizations "oauth2"
        responseMessages(response400, response403, response500))

    post("/", operation(newArticle)) {
      authUser.assertHasId()
      authRole.assertHasWritePermission()
      writeService.newArticle(extract[NewArticle](request.body)) match {
        case Success(article) => Created(body=article)
        case Failure(exception) => errorHandler(exception)
      }
    }

    val queueDraftForPublishing =
      (apiOperation[Article]("queueDraftForPublishing ")
        summary "Queue the article for publishing"
        notes "Queue the article for publishing"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id"),
        pathParam[Long]("article_id").description("Id of the article that is to be published"),
        bodyParam[ArticleStatus]
      )
        authorizations "oauth2"
        responseMessages(response400, response403, response404, response500))

    put("/:article_id/publish", operation(queueDraftForPublishing)) {
      authRole.assertHasPublishPermission()
      writeService.queueArticleForPublish(long("article_id")) match {
        case Success(_) => NoContent()
        case Failure(e) => errorHandler(e)
      }
    }

    val updateArticle =
      (apiOperation[Article]("updateArticle")
        summary "Update an existing article"
        notes "Update an existing article"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id"),
        pathParam[Long]("article_id").description("Id of the article that is to be updated"),
        bodyParam[UpdatedArticle]
      )
        authorizations "oauth2"
        responseMessages(response400, response403, response404, response500))

    patch("/:article_id", operation(updateArticle)) {
      authUser.assertHasId()
      authRole.assertHasWritePermission()
      writeService.updateArticle(long("article_id"), extract[UpdatedArticle](request.body)) match {
        case Success(article) => Ok(body=article)
        case Failure(exception) => errorHandler(exception)
      }
    }

    val validateArticle =
      (apiOperation[Unit]("validateArticle")
        summary "Validate an article"
        notes "Validate an article"
        parameters(
        headerParam[Option[String]]("X-Correlation-ID").description("User supplied correlation-id"),
        pathParam[Long]("article_id").description("Id of the article that is to be validated")
      )
        authorizations "oauth2"
        responseMessages(response400, response403, response404, response500))

    put("/:article_id/validate", operation(validateArticle)) {
      contentValidator.validateArticleApiArticle(long("article_id")) match {
        case Success(_) => Ok()
        case Failure(ex) => errorHandler(ex)
      }
    }

  }
}
