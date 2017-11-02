/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi

import javax.sql.DataSource
import org.flywaydb.core.Flyway

object DBMigrator {
  def migrate(datasource: DataSource) = {
    val flyway = new Flyway()
    flyway.setDataSource(datasource)
    flyway.migrate()
  }
}
