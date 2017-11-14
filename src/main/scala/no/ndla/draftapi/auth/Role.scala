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
    val DraftRoleWithPublishAccess = "drafts:publish"
    val ArticleRoleWithWriteAccess = "articles:write"
    val ArticleRoleWithPublishAccess = "articles:publish"

    def assertHasRole(role: String): Unit = {
      if (!AuthUser.hasRole(role))
        throw new AccessDeniedException("User is missing required role to perform this operation")
    }

    def hasRoles(roles: Set[String]): Boolean = roles.map(AuthUser.hasRole).forall(identity)

    def assertHasRoles(roles: Set[String]): Unit = {
      if (!hasRoles(roles))
        throw new AccessDeniedException("user is missing required role(s) to perform this operation")
    }

    def requiredRolesForStatusUpdate(statusToSet: Set[ArticleStatus.Value], article: domain.Article): Set[String] = {
      if (statusToSet.contains(ArticleStatus.QUEUED_FOR_PUBLISHING) || article.status.contains(ArticleStatus.QUEUED_FOR_PUBLISHING))
        authRole.setPublishStatusRoles
      else
        authRole.updateDraftRoles
    }

    val updateDraftRoles: Set[String] = Set(DraftRoleWithWriteAccess)
    val setStatusRoles: Set[String] = Set(DraftRoleWithWriteAccess)
    val setPublishStatusRoles: Set[String] = setStatusRoles ++ Set(DraftRoleWithPublishAccess)
    val publishToArticleApiRoles: Set[String] = setPublishStatusRoles ++ Set(ArticleRoleWithWriteAccess, ArticleRoleWithPublishAccess)
  }

}


