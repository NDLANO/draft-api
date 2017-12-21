/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi

import no.ndla.draftapi.auth.Role
import org.scalatra.ScalatraServlet
import org.scalatra.swagger._

class ResourcesApp(implicit val swagger: Swagger) extends ScalatraServlet with NativeSwaggerBase {
  get("/") {
    renderSwagger2(swagger.docs.toList)
  }
}

object DraftApiInfo {
  val apiInfo = ApiInfo(
    "Draft API",
    "Documentation for the Draft API of NDLA.no",
    "http://ndla.no",
    DraftApiProperties.ContactEmail,
    "GPL v3.0",
    "http://www.gnu.org/licenses/gpl-3.0.en.html")
}

class DraftSwagger extends Swagger("2.0", "0.8", DraftApiInfo.apiInfo) with Role {
  def createRoleInTestEnv(role: String): String = role.replace(":", "-test:")

  addAuthorization(OAuth(List(createRoleInTestEnv(authRole.DraftRoleWithWriteAccess), createRoleInTestEnv(authRole.DraftRoleWithPublishAccess), createRoleInTestEnv(authRole.ArticleRoleWithPublishAccess)), List(ImplicitGrant(LoginEndpoint(DraftApiProperties.Auth0LoginEndpoint), "access_token"))))
}
