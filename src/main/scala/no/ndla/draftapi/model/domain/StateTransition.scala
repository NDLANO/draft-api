/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import no.ndla.draftapi.auth.{Role, UserInfo}

case class StateTransition(from: ArticleStatus.Value,
                           to: ArticleStatus.Value,
                           otherStatesToKeepOnTransition: Set[ArticleStatus.Value] = Set(ArticleStatus.IMPORTED),
                           addCurrentStateToOthersOnTransition: Boolean = true,
                           requiredRoles: Set[Role.Value] = UserInfo.WriteRoles)
