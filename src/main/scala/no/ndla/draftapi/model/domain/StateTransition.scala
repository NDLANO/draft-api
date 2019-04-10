/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import no.ndla.draftapi.auth.{Role, UserInfo}
import no.ndla.draftapi.model.domain

import scala.util.{Success, Try}
import scala.language.implicitConversions

case class StateTransition(from: ArticleStatus.Value,
                           to: ArticleStatus.Value,
                           otherStatesToKeepOnTransition: Set[ArticleStatus.Value],
                           sideEffect: domain.Article => Try[domain.Article],
                           addCurrentStateToOthersOnTransition: Boolean,
                           requiredRoles: Set[Role.Value]) {

  def keepCurrentOnTransition: StateTransition = copy(addCurrentStateToOthersOnTransition = true)
  def keepStates(toKeep: Set[ArticleStatus.Value]): StateTransition = copy(otherStatesToKeepOnTransition = toKeep)
  def withSideEffect(sideEffect: domain.Article => Try[domain.Article]): StateTransition = copy(sideEffect = sideEffect)
  def require(roles: Set[Role.Value]): StateTransition = copy(requiredRoles = roles)
}

object StateTransition {
  implicit def tupleToStateTransition(fromTo: (ArticleStatus.Value, ArticleStatus.Value)): StateTransition = {
    val (from, to) = fromTo
    StateTransition(from,
                    to,
                    Set(ArticleStatus.IMPORTED, ArticleStatus.PUBLISHED),
                    Success.apply,
                    addCurrentStateToOthersOnTransition = false,
                    UserInfo.WriteRoles)
  }
}
