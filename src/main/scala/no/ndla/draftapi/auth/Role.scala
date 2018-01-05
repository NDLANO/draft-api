/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.auth

import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.model.api.AccessDeniedException
import no.ndla.network.AuthUser

trait Role {
  val authRole: AuthRole

  class AuthRole {

    private def hasRoles(roles: String*): Boolean = roles.map(AuthUser.hasRole).forall(identity)

    private def assert(hasRoles: Boolean): Unit = {
      if (!hasRoles)
        throw new AccessDeniedException("user is missing required role(s) to perform this operation")
    }

    def assertHasWritePermission(): Unit = assert(hasRoles(DraftApiProperties.DraftRoleWithWriteAccess))
    def hasPublishPermission(): Boolean = hasRoles(DraftApiProperties.DraftRoleWithWriteAccess, DraftApiProperties.DraftRoleWithPublishAccess)
    def assertHasPublishPermission(): Unit = assert(hasPublishPermission())
    def assertHasArticleApiPublishPermission(): Unit = assert(hasRoles(DraftApiProperties.DraftRoleWithWriteAccess, DraftApiProperties.DraftRoleWithPublishAccess, DraftApiProperties.ArticleRoleWithPublishAccess))
  }

}


