/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.auth

import no.ndla.network.AuthUser

case class UserInfo(id: String, roles: Set[Role.Value]) {
  def isAdmin: Boolean = roles.contains(Role.ADMIN)
  def canPublish: Boolean = Set(Role.WRITE, Role.SET_TO_PUBLISH).subsetOf(roles)
  def canWrite: Boolean = roles.contains(Role.WRITE)
  def canRead: Boolean = canWrite
}

object UserInfo {
  val UnauthorizedUser = UserInfo("unauthorized", Set.empty)

  def apply(name: String): UserInfo = UserInfo(name, AuthUser.getRoles.flatMap(Role.valueOf).toSet)

  def get: Option[UserInfo] = AuthUser.get.map(UserInfo.apply)
}

trait User {
  val user: User

  class User {
    def getUser: UserInfo = UserInfo.get.getOrElse(UserInfo.UnauthorizedUser)
  }
}
