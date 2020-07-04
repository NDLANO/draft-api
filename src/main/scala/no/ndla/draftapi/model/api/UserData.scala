package no.ndla.draftapi.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

@ApiModel(description = "Information about user data")
case class UserData(
     @(ApiModelProperty @field)(description = "The unique id of the user") id: Long,
     @(ApiModelProperty @field)(description = "The auth0 id of the user") userId: String,
     @(ApiModelProperty @field)(description = "The content of the user's data") savedSearches: Seq[String])
