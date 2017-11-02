/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.auth

import no.ndla.draftapi.model.api.AccessDeniedException
import no.ndla.network.AuthUser

trait Role {

  val authRole: AuthRole

  class AuthRole {
    def assertHasRole(role: String): Unit = {
      if (!AuthUser.hasRole(role))
        throw new AccessDeniedException("User is missing required role to perform this operation")
    }
  }

}


