/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.domain.{ArticleStatus, StateTransition}

object StateTransitionRules {

  val stateTransitions = Set(
    StateTransition(ArticleStatus.DRAFT, ArticleStatus.DRAFT, addToOthers = false),
    StateTransition(ArticleStatus.DRAFT, ArticleStatus.PROPOSAL, addToOthers = false),
  )

  private def getTransition(from: ArticleStatus.Value, to: ArticleStatus.Value): Option[StateTransition] = {
    stateTransitions.find(transition => transition.from == from && transition.to == to)
  }

  def transitionIsLegal(from: ArticleStatus.Value, to: ArticleStatus.Value, user: UserInfo): Boolean = {
    getTransition(from, to) match {
      case Some(t) if t.adminRequired && !user.isAdmin => false
      case None                                        => false
      case Some(_)                                     => true
    }
  }

  def addStateToOther(other: Set[ArticleStatus.Value],
                      from: ArticleStatus.Value,
                      to: ArticleStatus.Value): Set[ArticleStatus.Value] = {
    getTransition(from, to) match {
      case Some(transition: StateTransition) if transition.addToOthers => other ++ Set(from)
      case _                                                           => other
    }
  }
}
