/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain
import io.searchbox.core

case class SearchResult(totalCount: Long, page: Int, pageSize: Int, language: String, response: core.SearchResult)

