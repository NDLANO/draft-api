/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

case class ArticleSummary(id: Long, title: Seq[ArticleTitle], visualElement: Seq[VisualElement], introduction: Seq[ArticleIntroduction], url: String, license: String)
