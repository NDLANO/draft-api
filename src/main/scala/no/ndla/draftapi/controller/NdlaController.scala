/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import javax.servlet.http.HttpServletRequest

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.ComponentRegistry
import no.ndla.draftapi.DraftApiProperties.{CorrelationIdHeader, CorrelationIdKey}
import no.ndla.draftapi.model.api.{
  AccessDeniedException,
  ArticlePublishException,
  ArticleStatusException,
  Error,
  NotFoundException,
  OptimisticLockException,
  ResultWindowTooLargeException,
  ValidationError
}
import no.ndla.draftapi.model.domain.emptySomeToNone
import no.ndla.network.model.HttpRequestException
import no.ndla.network.{ApplicationUrl, AuthUser, CorrelationID}
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.apache.logging.log4j.ThreadContext
import org.elasticsearch.index.IndexNotFoundException
import org.json4s.native.Serialization.read
import org.json4s.{DefaultFormats, Formats}
import org.postgresql.util.PSQLException
import org.scalatra.json.NativeJsonSupport
import org.scalatra._
import org.scalatra.swagger.SwaggerSupport
import org.scalatra.util.NotNothing

import scala.util.{Failure, Success, Try}

abstract class NdlaController extends ScalatraServlet with NativeJsonSupport with LazyLogging with SwaggerSupport {
  protected implicit override val jsonFormats: Formats = DefaultFormats

  before() {
    contentType = formats("json")
    CorrelationID.set(Option(request.getHeader(CorrelationIdHeader)))
    ThreadContext.put(CorrelationIdKey, CorrelationID.get.getOrElse(""))
    ApplicationUrl.set(request)
    AuthUser.set(request)
    logger.info("{} {}{}",
                request.getMethod,
                request.getRequestURI,
                Option(request.getQueryString).map(s => s"?$s").getOrElse(""))
  }

  after() {
    CorrelationID.clear()
    ThreadContext.remove(CorrelationIdKey)
    AuthUser.clear()
    ApplicationUrl.clear
  }

  error {
    case a: AccessDeniedException          => Forbidden(body = Error(Error.ACCESS_DENIED, a.getMessage))
    case v: ValidationException            => BadRequest(body = ValidationError(messages = v.errors))
    case as: ArticleStatusException        => BadRequest(body = Error(Error.VALIDATION, as.getMessage))
    case e: IndexNotFoundException         => InternalServerError(body = Error.IndexMissingError)
    case n: NotFoundException              => NotFound(body = Error(Error.NOT_FOUND, n.getMessage))
    case o: OptimisticLockException        => Conflict(body = Error(Error.RESOURCE_OUTDATED, o.getMessage))
    case rw: ResultWindowTooLargeException => UnprocessableEntity(body = Error(Error.WINDOW_TOO_LARGE, rw.getMessage))
    case pf: ArticlePublishException       => BadRequest(body = Error(Error.PUBLISH, pf.getMessage))
    case _: PSQLException =>
      ComponentRegistry.connectToDatabase()
      InternalServerError(Error(Error.DATABASE_UNAVAILABLE, Error.DATABASE_UNAVAILABLE_DESCRIPTION))
    case h: HttpRequestException =>
      h.httpResponse match {
        case Some(resp) if resp.is4xx => BadRequest(body = resp.body)
        case _ =>
          logger.error(s"Problem with remote service: ${h.getMessage}")
          BadGateway(body = Error.GenericError)
      }
    case t: Throwable =>
      logger.error(Error.GenericError.toString, t)
      InternalServerError(body = Error.GenericError)
  }

  case class Param(paramName: String, description: String)

  protected val correlationId = Param("X-Correlation-ID", "User supplied correlation-id. May be omitted.")
  protected val pageNo = Param("page", "The page number of the search hits to display.")
  protected val pageSize = Param("page-size", "The number of search hits to display for each page.")
  protected val sort = Param(
    "sort",
    """The sorting used on results.
             The following are supported: relevance, -relevance, title, -title, lastUpdated, -lastUpdated, id, -id.
             Default is by -relevance (desc) when query is set, and title (asc) when query is empty.""".stripMargin
  )
  protected val deprecatedNodeId = Param("deprecated_node_id", "Id of deprecated NDLA node")
  protected val language = Param("language", "The ISO 639-1 language code describing language.")
  protected val license = Param("license", "Return only results with provided license.")
  protected val fallback = Param("fallback", "Fallback to existing language if language is specified.")

  protected def asQueryParam[T: Manifest: NotNothing](param: Param) =
    queryParam[T](param.paramName).description(param.description)
  protected def asHeaderParam[T: Manifest: NotNothing](param: Param) =
    headerParam[T](param.paramName).description(param.description)
  protected def asPathParam[T: Manifest: NotNothing](param: Param) =
    pathParam[T](param.paramName).description(param.description)

  def long(paramName: String)(implicit request: HttpServletRequest): Long = {
    val paramValue = params(paramName)
    paramValue.forall(_.isDigit) match {
      case true => paramValue.toLong
      case false =>
        throw new ValidationException(
          errors = Seq(ValidationMessage(paramName, s"Invalid value for $paramName. Only digits are allowed.")))
    }
  }

  def paramOrNone(paramName: String)(implicit request: HttpServletRequest): Option[String] = {
    params.get(paramName).map(_.trim).filterNot(_.isEmpty())
  }

  def paramOrDefault(paramName: String, default: String)(implicit request: HttpServletRequest): String = {
    paramOrNone(paramName).getOrElse(default)
  }

  def intOrNone(paramName: String)(implicit request: HttpServletRequest): Option[Int] =
    paramOrNone(paramName).flatMap(p => Try(p.toInt).toOption)

  def intOrDefault(paramName: String, default: Int): Int = intOrNone(paramName).getOrElse(default)

  def paramAsListOfString(paramName: String)(implicit request: HttpServletRequest): List[String] = {
    emptySomeToNone(params.get(paramName)) match {
      case None        => List.empty
      case Some(param) => param.split(",").toList.map(_.trim)
    }
  }

  def paramAsListOfLong(paramName: String)(implicit request: HttpServletRequest): List[Long] = {
    val strings = paramAsListOfString(paramName)
    strings.headOption match {
      case None => List.empty
      case Some(_) =>
        if (!strings.forall(entry => entry.forall(_.isDigit))) {
          throw new ValidationException(
            errors =
              Seq(ValidationMessage(paramName, s"Invalid value for $paramName. Only (list of) digits are allowed.")))
        }
        strings.map(_.toLong)
    }
  }

  def booleanOrNone(paramName: String)(implicit request: HttpServletRequest): Option[Boolean] =
    paramOrNone(paramName).flatMap(p => Try(p.toBoolean).toOption)

  def booleanOrDefault(paramName: String, default: Boolean)(implicit request: HttpServletRequest): Boolean =
    booleanOrNone(paramName).getOrElse(default)

  def extract[T](json: String)(implicit mf: scala.reflect.Manifest[T]): T = {
    Try {
      read[T](json)
    } match {
      case Failure(e) => {
        logger.error(e.getMessage, e)
        throw new ValidationException(errors = Seq(ValidationMessage("body", e.getMessage)))
      }
      case Success(data) => data
    }
  }

}
