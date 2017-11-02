/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.converters

import no.ndla.draftapi.model.domain.Attributes
import no.ndla.draftapi.validation.HtmlTools
import no.ndla.draftapi.{TestEnvironment, UnitSuite}

class HtmlToolsTest extends UnitSuite with TestEnvironment {
  test("embed tag should be an allowed tag and contain data attributes") {
    HtmlTools.isTagValid("embed")

    val dataAttrs = Attributes.values.map(_.toString).filter(x => x.startsWith("data-") && x != Attributes.DataType.toString)
    val legalEmbedAttrs = HtmlTools.legalAttributesForTag("embed")

    dataAttrs.foreach(x => legalEmbedAttrs should contain(x))
  }

  test("That isAttributeKeyValid returns false for illegal attributes") {
    HtmlTools.isAttributeKeyValid("data-random-junk", "td") should equal(false)
  }

  test("That isAttributeKeyValid returns true for legal attributes") {
    HtmlTools.isAttributeKeyValid("align", "td") should equal(true)
  }

  test("That isTagValid returns false for illegal tags") {
    HtmlTools.isTagValid("yodawg") should equal(false)
  }

  test("That isTagValid returns true for legal attributes") {
    HtmlTools.isTagValid("section") should equal(true)
  }

}
