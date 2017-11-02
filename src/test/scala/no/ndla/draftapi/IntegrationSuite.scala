/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi

import javax.sql.DataSource

import no.ndla.network.secrets.PropertyKeys
import org.postgresql.ds.PGPoolingDataSource

abstract class IntegrationSuite extends UnitSuite {

  setEnv(PropertyKeys.MetaUserNameKey, "postgres")
  setEnvIfAbsent(PropertyKeys.MetaPasswordKey, "hemmelig")
  setEnv(PropertyKeys.MetaResourceKey, "postgres")
  setEnv(PropertyKeys.MetaServerKey, "127.0.0.1")
  setEnv(PropertyKeys.MetaPortKey, "5432")
  setEnv(PropertyKeys.MetaSchemaKey, "draftapitest")


  lazy val getDataSource: DataSource = {
    val datasource = new PGPoolingDataSource()
    datasource.setUser(DraftApiProperties.MetaUserName)
    datasource.setPassword(DraftApiProperties.MetaPassword)
    datasource.setDatabaseName(DraftApiProperties.MetaResource)
    datasource.setServerName(DraftApiProperties.MetaServer)
    datasource.setPortNumber(DraftApiProperties.MetaPort)
    datasource.setInitialConnections(DraftApiProperties.MetaInitialConnections)
    datasource.setMaxConnections(DraftApiProperties.MetaMaxConnections)
    datasource.setCurrentSchema(DraftApiProperties.MetaSchema)
    datasource
  }
}
