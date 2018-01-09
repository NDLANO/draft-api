/*
 * Part of NDLA draft_api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

@ApiModel(description = "Meta description of the article")
case class ArticleMetaImage(@(ApiModelProperty@field)(description = "The meta image") metaDescription: String,
                            @(ApiModelProperty@field)(description = "The ISO 639-1 language code describing which article translation this meta image belongs to") language: String)
