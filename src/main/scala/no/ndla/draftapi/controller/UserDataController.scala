package no.ndla.draftapi.controller

import no.ndla.draftapi.auth.User
import no.ndla.draftapi.model.api.{Error, NewUserData, UserData}
import no.ndla.draftapi.service.{ReadService, WriteService}
import org.scalatra.{Created, NotFound}
import org.scalatra.swagger.{ResponseMessage, Swagger}

import scala.util.{Failure, Success}

trait UserDataController {
  this: ReadService
    with WriteService
    with User =>
  val userDataController: UserDataController

  class UserDataController(implicit val swagger: Swagger) extends NdlaController {
    val response400 = ResponseMessage(400, "Validation Error", Some("ValidationError"))
    val response403 = ResponseMessage(403, "Access Denied", Some("Error"))
    val response404 = ResponseMessage(404, "Not found", Some("Error"))
    val response500 = ResponseMessage(500, "Unknown error", Some("Error"))

    private val query =
      Param[Option[String]]("query", "Return only user data with content matching the specified query.")
    private val userId = Param[String]("user_id", "Id of the user that is to be fetched") // sjekk om denne er rett

    get("/:user_id", // TODO blir dette rett kall
      operation(
        apiOperation[UserData]("getSavedSearches")
          summary "Retrieves a list of an users saved searches"
          description "Retrieves a list of an users saved searches"
          parameters(
          asHeaderParam(correlationId),
          asQueryParam(query)
        )
          responseMessages response500
          authorizations "oauth2")
    ) {
      val userInfo = user.getUser // TODO is userId available from here?

      doOrAccessDenied(userInfo.canWrite) { // TODO er dette rett sjekk
        val userId = paramOrNone("user_id") // TODO usikker på om user_id er rett variabel
        val userData = readService.getUserData(userId) //TODO se over parameter i funksjonen

        if (userData.isEmpty) {
          NotFound(body = Error(Error.NOT_FOUND, s"No user data for user $userId was found"))
        } else {
          userData
        }
      }
    }

    post(
      "/:user_id", // TODO blir dette rett kall
      operation(
        apiOperation[UserData]("createSavedSearches")
          summary "Create saved searches for user"
          description "Create saved searches for user"
          parameters(
          asHeaderParam(correlationId),
          bodyParam[UserData]
        )
          authorizations "oauth2"
          responseMessages(response400, response403, response500)) // TODO se om disse blir rett
    ) {
      val userInfo = user.getUser
      doOrAccessDenied(userInfo.canWrite) {
        val externalId = paramAsListOfString("externalId")

        extract[NewUserData](request.body).flatMap(
          writeService.newUserData(_, userInfo)) match {
          case Success(userData) => Created(body = userData)
          case Failure(exception) => errorHandler(exception)
        }
      }
    }


    patch(
      "/:user_id",
      operation(
        apiOperation[UserData]("updateSavedSearches")
          summary "Update users saved searches"
          description "Update users saved searches"
          parameters(
          asHeaderParam[Option[String]](correlationId),
          asPathParam[String](userId),
          bodyParam[UserData]
        )
          authorizations "oauth2"
          responseMessages(response400, response403, response404, response500))
    ) {
      val userInfo = user.getUser
      doOrAccessDenied(userInfo.canWrite) {
        val userId = this.userId.paramName
        val updatedSavedSearches = extract[UserData](request.body)

        updateUserData.flatMap( // TODO lag funksjon, finn ut hvilke parameter som trengs
          writeService.updateUserData(userId, _, updatedSavedSearches)) match {
          case Success(userData) => Created(body = userData)
          case Failure(exception) => errorHandler(exception)
        }
    }
  }

}