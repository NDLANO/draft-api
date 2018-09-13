/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class StateTransition(from: ArticleStatus.Value,
                           to: ArticleStatus.Value,
                           addToOthers: Boolean = true,
                           adminRequired: Boolean = false)
