/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi

import org.joda.time.{DateTime, DateTimeUtils}
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

abstract class UnitSuite
    extends FunSuite
    with Matchers
    with OptionValues
    with Inside
    with Inspectors
    with MockitoSugar
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  setEnv("NDLA_ENVIRONMENT", "local")
  setEnv("ENABLE_JOUBEL_H5P_OEMBED", "true")

  setEnv("SEARCH_SERVER", "some-server")
  setEnv("SEARCH_REGION", "some-region")
  setEnv("RUN_WITH_SIGNED_SEARCH_REQUESTS", "false")
  setEnv("SEARCH_INDEX_NAME", "draft-integration-test-index")
  setEnv("AGREEMENT_SEARCH_INDEX_NAME", "agreement-integration-test-index")

  setEnv("AUDIO_API_URL", "localhost:30014")
  setEnv("IMAGE_API_URL", "localhost:30001")

  setEnv("NDLA_BRIGHTCOVE_ACCOUNT_ID", "some-account-id")
  setEnv("NDLA_BRIGHTCOVE_PLAYER_ID", "some-player-id")

  def setEnv(key: String, value: String) = env.put(key, value)

  def setEnvIfAbsent(key: String, value: String) = env.putIfAbsent(key, value)

  private def env = {
    val field = System.getenv().getClass.getDeclaredField("m")
    field.setAccessible(true)
    field.get(System.getenv()).asInstanceOf[java.util.Map[java.lang.String, java.lang.String]]
  }

  def withFrozenTime(time: DateTime = new DateTime())(toExecute: => Any) = {
    DateTimeUtils.setCurrentMillisFixed(time.getMillis)
    toExecute
    DateTimeUtils.setCurrentMillisSystem()
  }
}
