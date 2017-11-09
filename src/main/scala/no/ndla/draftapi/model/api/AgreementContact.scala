/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

@ApiModel(description = "Information about a contact")
case class AgreementContact(@(ApiModelProperty@field)(description = "Contact name") name: String,
                            @(ApiModelProperty@field)(description = "Contact email") email: String)
