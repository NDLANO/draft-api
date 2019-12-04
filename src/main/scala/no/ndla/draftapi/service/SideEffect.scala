package no.ndla.draftapi.service

import no.ndla.draftapi.model.domain

import scala.util.{Success, Try}

object SideEffect {
  type SideEffect = (domain.Article, Boolean) => Try[domain.Article]
  def none: SideEffect = (article, isImported) => Success(article)
  def fromOutput(output: Try[domain.Article]): SideEffect = (_, _) => output
}
