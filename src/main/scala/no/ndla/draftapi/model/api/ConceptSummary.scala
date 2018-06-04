/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}
import scala.annotation.meta.field

@ApiModel(description = "Information about the concept")
case class ConceptSummary(
    @(ApiModelProperty @field)(description = "The unique id of the article") id: Long,
    @(ApiModelProperty @field)(description = "Available titles for the article") title: ConceptTitle,
    @(ApiModelProperty @field)(description = "The content of the article in available languages") content: ConceptContent,
    @(ApiModelProperty @field)(description = "All available languages of the current concept") supportedLanguages: Seq[
      String])
