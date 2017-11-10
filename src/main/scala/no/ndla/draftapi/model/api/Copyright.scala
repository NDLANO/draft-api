/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import java.util.Date

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

@ApiModel(description = "Description of copyright information")
case class Copyright(@(ApiModelProperty@field)(description = "Describes the license of the article") license: Option[License],
                     @(ApiModelProperty@field)(description = "Reference to where the article is procured") origin: Option[String],
                     @(ApiModelProperty@field)(description = "List of creators") creators: Seq[Author],
                     @(ApiModelProperty@field)(description = "List of processors") processors: Seq[Author],
                     @(ApiModelProperty@field)(description = "List of rightsholders") rightsholders: Seq[Author],
                     @(ApiModelProperty@field)(description = "Reference to agreement id") agreement: Option[Long],
                     @(ApiModelProperty@field)(description = "List of rightsholders") validFrom: Option[Date],
                     @(ApiModelProperty@field)(description = "List of rightsholders") validTo: Option[Date])
