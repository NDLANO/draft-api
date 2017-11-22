/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.auth

import no.ndla.draftapi.model.api.AccessDeniedException
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.domain.ArticleStatus
import no.ndla.network.AuthUser

trait Role {
  val authRole: AuthRole

  class AuthRole {
    val DraftRoleWithWriteAccess = "drafts:write"
    val DraftRoleWithPublishAccess = "drafts:set_to_publish"
    val ArticleRoleWithPublishAccess = "articles:publish"

    def hasRoles(roles: Set[String]): Boolean = roles.map(AuthUser.hasRole).forall(identity)

    def assertHasRoles(roles: String*): Unit = {
      if (!hasRoles(roles.toSet))
        throw new AccessDeniedException("user is missing required role(s) to perform this operation")
    }

    def assertHasWritePermission(): Unit = assertHasRoles(DraftRoleWithWriteAccess)
    def assertHasPublishPermission(): Unit = assertHasRoles(DraftRoleWithWriteAccess, DraftRoleWithPublishAccess)
    def assertHasArticleApiPublishPermission(): Unit = assertHasRoles(DraftRoleWithWriteAccess, DraftRoleWithPublishAccess, ArticleRoleWithPublishAccess)
  }

}


