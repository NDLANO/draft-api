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

case class StateTransition(from: ArticleStatus.Value,
                           to: ArticleStatus.Value,
                           otherStatesToKeepOnTransition: Set[ArticleStatus.Value] = Set(ArticleStatus.IMPORTED),
                           sideEffect: domain.Article => Try[domain.Article] = a => Success(a),
                           addCurrentStateToOthersOnTransition: Boolean = true,
                           requiredRoles: Set[Role.Value] = UserInfo.WriteRoles)
