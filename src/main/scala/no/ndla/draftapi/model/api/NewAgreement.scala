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

@ApiModel(description = "Information about the agreement")
case class NewAgreement(@(ApiModelProperty@field)(description = "Titles for the agreement") title: String,
                        @(ApiModelProperty@field)(description = "The content of the agreement") content: String,
                        @(ApiModelProperty@field)(description = "Internal contact of agreement") internalContact: AgreementContact,
                        @(ApiModelProperty@field)(description = "Supplier of agreement") supplier: AgreementContact,
                        @(ApiModelProperty@field)(description = "Describes the copyright information for the article") copyright: Copyright,
                        @(ApiModelProperty@field)(description = "The date from which the agreement is valid") validFrom: Date,
                        @(ApiModelProperty@field)(description = "The date to which the agreement is valid") validTo: Date,
                        @(ApiModelProperty@field)(description = "The languages this article supports") language: String
                    )
