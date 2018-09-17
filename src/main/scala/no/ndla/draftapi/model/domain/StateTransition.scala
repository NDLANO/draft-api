/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class StateTransition(from: ArticleStatus.Value,
                           to: ArticleStatus.Value,
                           otherStatesToKeepOnTransition: Set[ArticleStatus.Value] = Set(ArticleStatus.IMPORTED),
                           addCurrentStateToOthersOnTransition: Boolean = true,
                           adminRequired: Boolean = false)
