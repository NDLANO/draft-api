/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import io.searchbox.client.JestResult

class NdlaSearchException(jestResponse: JestResult) extends RuntimeException(jestResponse.getErrorMessage) {
  def getResponse: JestResult = jestResponse
}
