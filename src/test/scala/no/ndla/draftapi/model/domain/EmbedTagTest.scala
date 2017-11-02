/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import no.ndla.draftapi.{TestEnvironment, UnitSuite}

class EmbedTagTest extends UnitSuite with TestEnvironment {

  test("Rules for all resource types should be defined") {
    val resourceTypesFromConfigFile = EmbedTag.attributeRules.keys
    val resourceTypesFromEnumDeclaration = ResourceType.values

    resourceTypesFromEnumDeclaration should equal (resourceTypesFromConfigFile)
  }

  test("data-resource should be required for all resource types") {
    val resourceTypesFromConfigFile = EmbedTag.attributeRules.keys

    resourceTypesFromConfigFile.foreach(resType =>
      EmbedTag.attributesForResourceType(resType).required should contain(Attributes.DataResource)
    )
  }

}
