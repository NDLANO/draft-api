/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import java.util.Date

case class Copyright(
                      license: String,
                      origin: String,
                      creators: Seq[Author],
                      processors: Seq[Author],
                      rightsholders: Seq[Author],
                      agreement: Option[Long],
                      validFrom: Option[Date],
                      validTo: Option[Date]
                    )
