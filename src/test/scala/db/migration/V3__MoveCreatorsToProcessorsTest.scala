/*
 * Part of NDLA draft_api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package db.migration

import no.ndla.draftapi.{TestEnvironment, UnitSuite}

class V3__MoveCreatorsToProcessorsTest extends UnitSuite with TestEnvironment {
  val migration = new V3__MoveCreatorsToProcessors

  test("migration should move editorials from creators to processors") {
    val before = """{"copyright":{"license":"by-sa","creators":[{"type":"editorial","name":"Henrik"},{"type":"writer","name":"Henrik"}],"processors":[],"rightsholders"[]}}"""
    val expected = """{"copyright":{"license":"by-sa","creators":[{"type":"writer","name":"Henrik"}],"processors":[{"type":"editorial","name":"Henrik"}],"rightsholders":[]}}"""

    val res = migration.convertArticleUpdate(before)
    res should equal(expected)
  }

  test("migration not do anyhting if the document is already is converted") {
    val original = """{}"""

    migration.convertArticleUpdate(original) should equal(original)
  }

}
