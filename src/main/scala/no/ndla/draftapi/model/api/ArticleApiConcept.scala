/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.api

import java.util.Date

case class ArticleApiConcept(title: Seq[ArticleApiConceptTitle],
                             content: Seq[ArticleApiConceptContent],
                             copyright: Option[ArticleApiCopyright],
                             created: Date,
                             updated: Date)
case class ArticleApiConceptTitle(title: String, language: String)
case class ArticleApiConceptContent(content: String, language: String)
