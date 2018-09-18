/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.auth

import no.ndla.network.AuthUser

case class UserInfo(id: String, roles: Set[Role.Value]) {
  def isAdmin: Boolean = hasRoles(UserInfo.AdminRoles)
  def canSetPublish: Boolean = hasRoles(UserInfo.SetPublishRoles)
  def canWrite: Boolean = hasRoles(UserInfo.WriteRoles)
  def canRead: Boolean = hasRoles(UserInfo.ReadRoles)

  def hasRoles(rolesToCheck: Set[Role.Value]): Boolean = rolesToCheck.subsetOf(roles)
}

object UserInfo {
  val UnauthorizedUser = UserInfo("unauthorized", Set.empty)

  val AdminRoles = Set(Role.ADMIN)
  val SetPublishRoles = Set(Role.WRITE, Role.SET_TO_PUBLISH)
  val WriteRoles = Set(Role.WRITE)
  val ReadRoles = Set(Role.WRITE)

  def apply(name: String): UserInfo = UserInfo(name, AuthUser.getRoles.flatMap(Role.valueOf).toSet)

  def get: Option[UserInfo] = (AuthUser.get orElse AuthUser.getClientId).map(UserInfo.apply)
}

trait User {
  val user: User

  class User {
    def getUser: UserInfo = UserInfo.get.getOrElse(UserInfo.UnauthorizedUser)
  }
}
