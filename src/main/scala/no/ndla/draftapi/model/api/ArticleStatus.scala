/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

@ApiModel(description = "The status of an article")
case class ArticleStatus(@(ApiModelProperty@field)(description = "The status of a single article", allowableValues = "CREATED,IMPORTED,DRAFT,SKETCH,USER_TEST,QUALITY_ASSURED,AWAITING_QUALITY_ASSURANCE") status: Set[String])
