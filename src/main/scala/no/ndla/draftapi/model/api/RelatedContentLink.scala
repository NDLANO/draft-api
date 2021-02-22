/*
 * Part of NDLA draft_api.
 * Copyright (C) 2021 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}
import scala.annotation.meta.field

@ApiModel(description = "Information about a library required to render the article")
case class RelatedContentLink(
    @(ApiModelProperty @field)(description = "The type of the library. E.g. CSS or JavaScript") title: String,
    @(ApiModelProperty @field)(description = "The full url to where the library can be downloaded") url: String)