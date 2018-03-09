/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.auth.{Role, User}
import no.ndla.draftapi.controller._
import no.ndla.draftapi.integration._
import no.ndla.draftapi.repository.{AgreementRepository, ConceptRepository, DraftRepository}
import no.ndla.draftapi.service._
import no.ndla.draftapi.service.search._
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.network.NdlaClient
import org.postgresql.ds.PGPoolingDataSource
import scalikejdbc.{ConnectionPool, DataSourceConnectionPool}

object ComponentRegistry
  extends DataSource
    with InternController
    with ConverterService
    with ConceptController
    with ConceptSearchService
    with ConceptIndexService
    with DraftController
    with AgreementController
    with HealthController
    with DraftRepository
    with AgreementRepository
    with ConceptRepository
    with Elastic4sClient
    with ReindexClient
    with ArticleSearchService
    with AgreementSearchService
    with IndexService
    with ArticleIndexService
    with AgreementIndexService
    with SearchService
    with LazyLogging
    with NdlaClient
    with SearchConverterService
    with ReadService
    with WriteService
    with ContentValidator
    with Clock
    with Role
    with User
    with ArticleApiClient {

  def connectToDatabase(): Unit = ConnectionPool.singleton(new DataSourceConnectionPool(dataSource))

  implicit val swagger = new DraftSwagger

  lazy val dataSource = new PGPoolingDataSource
  dataSource.setUser(DraftApiProperties.MetaUserName)
  dataSource.setPassword(DraftApiProperties.MetaPassword)
  dataSource.setDatabaseName(DraftApiProperties.MetaResource)
  dataSource.setServerName(DraftApiProperties.MetaServer)
  dataSource.setPortNumber(DraftApiProperties.MetaPort)
  dataSource.setInitialConnections(DraftApiProperties.MetaInitialConnections)
  dataSource.setMaxConnections(DraftApiProperties.MetaMaxConnections)
  dataSource.setCurrentSchema(DraftApiProperties.MetaSchema)
  connectToDatabase()

  lazy val internController = new InternController
  lazy val draftController = new DraftController
  lazy val agreementController = new AgreementController
  lazy val conceptController = new ConceptController
  lazy val resourcesApp = new ResourcesApp
  lazy val healthController = new HealthController

  lazy val draftRepository = new ArticleRepository
  lazy val conceptRepository = new ConceptRepository
  lazy val agreementRepository = new AgreementRepository

  lazy val articleSearchService = new ArticleSearchService
  lazy val articleIndexService = new ArticleIndexService
  lazy val agreementSearchService = new AgreementSearchService
  lazy val agreementIndexService = new AgreementIndexService
  lazy val conceptSearchService = new ConceptSearchService
  lazy val conceptIndexService = new ConceptIndexService

  lazy val converterService = new ConverterService
  lazy val contentValidator = new ContentValidator(allowEmptyLanguageField = false)
  lazy val importValidator = new ContentValidator(allowEmptyLanguageField = true)

  lazy val ndlaClient = new NdlaClient
  lazy val searchConverterService = new SearchConverterService
  lazy val readService = new ReadService
  lazy val writeService = new WriteService
  lazy val reindexClient = new ReindexClient

  lazy val e4sClient: NdlaE4sClient = Elastic4sClientFactory.getClient()

  lazy val clock = new SystemClock
  lazy val authRole = new AuthRole
  lazy val authUser = new AuthUser

  lazy val articleApiClient = new ArticleApiClient
}
