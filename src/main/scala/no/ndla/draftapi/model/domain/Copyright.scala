/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class Copyright(license: Option[String], origin: Option[String], authors: Seq[Author])
