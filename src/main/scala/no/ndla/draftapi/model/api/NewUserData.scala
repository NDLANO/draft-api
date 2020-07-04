package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

case class NewUserData()


@ApiModel(description = "Information about user data")
case class NewAgreement(
     @(ApiModelProperty @field)(description = "The unique id of the user") id: Long,
     @(ApiModelProperty @field)(description = "The auth0 id of the user") userId: String,
     @(ApiModelProperty @field)(description = "The content of the user's data") savedSearches: Seq[String])
