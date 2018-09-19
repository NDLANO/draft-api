/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import no.ndla.draftapi.model.domain
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.draftapi.model.domain.ArticleStatus._

import scala.util.Success

class StateTransitionRulesTest extends UnitSuite with TestEnvironment {
  import StateTransitionRules.doTransitionWithoutSideEffect

  val DraftStatus = domain.Status(DRAFT, Set.empty)
  val DraftArticle = TestData.sampleArticleWithByNcSa.copy(status = DraftStatus)

  test("doTransition should succeed when performing a legal transition") {
    val expected = domain.Status(PUBLISHED, Set.empty)
    val (Success(res), _) = doTransitionWithoutSideEffect(DraftArticle, PUBLISHED, TestData.userWIthAdminAccess)

    res.status should equal(expected)
  }

}
