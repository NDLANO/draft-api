/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi

import com.zaxxer.hikari.HikariDataSource
import no.ndla.network.secrets.PropertyKeys
import no.ndla.draftapi.integration.DataSource.getHikariDataSource
import org.testcontainers.elasticsearch.ElasticsearchContainer

import scala.util.{Failure, Try}

abstract class IntegrationSuite(withSearch: Boolean = false) extends UnitSuite {

  val elasticSearchContainer = if (withSearch) {
    Try {
      val esVersion = "6.3.2"
      val c = new ElasticsearchContainer(s"docker.elastic.co/elasticsearch/elasticsearch:$esVersion")
      c.start()
      c
    }
  } else { Failure(new RuntimeException("Search disabled for this IntegrationSuite")) }

  val elasticSearchHost = elasticSearchContainer.map(c => s"http://${c.getHttpHostAddress}")

  setEnv(PropertyKeys.MetaUserNameKey, "postgres")
  setEnvIfAbsent(PropertyKeys.MetaPasswordKey, "hemmelig")
  setEnv(PropertyKeys.MetaResourceKey, "postgres")
  setEnv(PropertyKeys.MetaServerKey, "127.0.0.1")
  setEnv(PropertyKeys.MetaPortKey, "5432")
  setEnv(PropertyKeys.MetaSchemaKey, "draftapitest")

  val testDataSource: Try[HikariDataSource] = Try { getHikariDataSource }
}
