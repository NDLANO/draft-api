/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}
import scala.annotation.meta.field
import java.util.Date

@ApiModel(description = "Information about the concept")
case class Concept(@(ApiModelProperty@field)(description = "The unique id of the concept") id: Long,
                   @(ApiModelProperty@field)(description = "Available titles for the concept") title: Option[ConceptTitle],
                   @(ApiModelProperty@field)(description = "The content of the concept") content: Option[ConceptContent],
                   @(ApiModelProperty@field)(description = "Describes the copyright information for the concept") copyright: Option[Copyright],
                   @(ApiModelProperty@field)(description = "When the concept was created") created: Date,
                   @(ApiModelProperty@field)(description = "When the concept was last updated") updated: Date,
                   @(ApiModelProperty@field)(description = "All available languages of the current concept") supportedLanguages: Set[String]
                  )
