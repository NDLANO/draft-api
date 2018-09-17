/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.auth.UserInfo
import no.ndla.draftapi.model.api.IllegalStatusStateTransition
import no.ndla.draftapi.model.domain
import no.ndla.draftapi.model.domain.{ArticleStatus, StateTransition}
import no.ndla.draftapi.model.domain.ArticleStatus._

import scala.util.{Failure, Success, Try}

object StateTransitionRules {

  val StateTransitions = Set(
    StateTransition(IMPORTED, DRAFT),
    StateTransition(DRAFT, DRAFT, addCurrentStateToOthersOnTransition = false),
    StateTransition(DRAFT, PROPOSAL, addCurrentStateToOthersOnTransition = false),
    StateTransition(DRAFT, PUBLISHED, addCurrentStateToOthersOnTransition = false, adminRequired = true),
    StateTransition(PROPOSAL, USER_TEST),
    StateTransition(PROPOSAL,
                    QUEUED_FOR_PUBLISHING,
                    Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
                    addCurrentStateToOthersOnTransition = false),
    StateTransition(PROPOSAL, PUBLISHED, adminRequired = true),
    StateTransition(PROPOSAL, AWAITING_QUALITY_ASSURANCE),
    StateTransition(USER_TEST, PROPOSAL),
    StateTransition(USER_TEST, AWAITING_QUALITY_ASSURANCE, Set(IMPORTED, PROPOSAL)),
    StateTransition(USER_TEST, PUBLISHED, adminRequired = true),
    StateTransition(AWAITING_QUALITY_ASSURANCE, USER_TEST, Set(IMPORTED, PROPOSAL)),
    StateTransition(AWAITING_QUALITY_ASSURANCE,
                    QUALITY_ASSURED,
                    Set(IMPORTED, USER_TEST),
                    addCurrentStateToOthersOnTransition = false),
    StateTransition(AWAITING_QUALITY_ASSURANCE, PUBLISHED, Set(IMPORTED, USER_TEST), adminRequired = true),
    StateTransition(QUALITY_ASSURED, QUEUED_FOR_PUBLISHING, Set(IMPORTED, USER_TEST)),
    StateTransition(QUALITY_ASSURED, PUBLISHED, Set(IMPORTED, USER_TEST), adminRequired = true),
    StateTransition(QUEUED_FOR_PUBLISHING,
                    PUBLISHED,
                    Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
                    addCurrentStateToOthersOnTransition = false,
                    adminRequired = true),
    StateTransition(PUBLISHED, DRAFT, addCurrentStateToOthersOnTransition = false),
    StateTransition(PUBLISHED, AWAITING_UNPUBLISHING, Set(IMPORTED, USER_TEST, QUALITY_ASSURED)),
    StateTransition(PUBLISHED, UNPUBLISHED, Set(IMPORTED, USER_TEST, QUALITY_ASSURED), adminRequired = true),
    StateTransition(AWAITING_UNPUBLISHING, DRAFT, addCurrentStateToOthersOnTransition = false),
    StateTransition(AWAITING_UNPUBLISHING,
                    PUBLISHED,
                    Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
                    addCurrentStateToOthersOnTransition = false,
                    adminRequired = true),
    StateTransition(AWAITING_UNPUBLISHING,
                    UNPUBLISHED,
                    Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
                    addCurrentStateToOthersOnTransition = false,
                    adminRequired = true),
    StateTransition(UNPUBLISHED,
                    PUBLISHED,
                    Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
                    addCurrentStateToOthersOnTransition = false,
                    adminRequired = true),
    StateTransition(UNPUBLISHED, DRAFT, addCurrentStateToOthersOnTransition = false),
    StateTransition(UNPUBLISHED,
                    ARCHIEVED,
                    Set(IMPORTED, USER_TEST, QUALITY_ASSURED),
                    addCurrentStateToOthersOnTransition = false,
                    adminRequired = true),
  )

  private def getTransition(from: ArticleStatus.Value,
                            to: ArticleStatus.Value,
                            user: UserInfo): Option[StateTransition] = {
    StateTransitions.find(transition => transition.from == from && transition.to == to) match {
      case None                                        => None
      case Some(t) if t.adminRequired && !user.isAdmin => None
      case Some(t)                                     => Some(t)
    }
  }

  def doTransition(current: domain.Status, to: ArticleStatus.Value, user: UserInfo): Try[domain.Status] = {
    getTransition(current.current, to, user) match {
      case Some(t) =>
        val currentToOther = if (t.addCurrentStateToOthersOnTransition) Set(current.current) else Set()
        val other = current.other.intersect(t.otherStatesToKeepOnTransition) ++ currentToOther
        Success(domain.Status(to, other))
      case None =>
        Failure(IllegalStatusStateTransition(s"Cannot go to $to when article is ${current.current}"))
    }
  }

}
