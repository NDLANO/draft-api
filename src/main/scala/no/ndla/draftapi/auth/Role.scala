/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.auth

object Role extends Enumeration {
  val WRITE, SET_TO_PUBLISH, ADMIN = Value

  def valueOf(s: String): Option[Role.Value] = {
    val role = s.split("drafts:")
    Role.values.find(_.toString == role.lastOption.getOrElse("").toUpperCase)
  }
}
