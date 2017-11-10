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
import no.ndla.draftapi.repository.{ConceptRepository, DraftRepository}
import no.ndla.draftapi.service._
import no.ndla.draftapi.service.search._
import no.ndla.draftapi.validation.ContentValidator
import no.ndla.network.NdlaClient
import org.scalatest.mockito.MockitoSugar

trait TestEnvironment
  extends ElasticClient
    with ArticleSearchService
    with ArticleIndexService
    with ConceptSearchService
    with ConceptIndexService
    with IndexService
    with SearchService
    with LazyLogging
    with DraftController
    with InternController
    with HealthController
    with ConceptController
    with DataSource
    with DraftRepository
    with ConceptRepository
    with MockitoSugar
    with ConverterService
    with NdlaClient
    with SearchConverterService
    with ReadService
    with WriteService
    with ContentValidator
    with Clock
    with User
    with Role
    with ArticleApiClient {
  val articleSearchService = mock[ArticleSearchService]
  val articleIndexService = mock[ArticleIndexService]
  val conceptSearchService = mock[ConceptSearchService]
  val conceptIndexService = mock[ConceptIndexService]

  val internController = mock[InternController]
  val draftController = mock[DraftController]
  val conceptController = mock[ConceptController]

  val healthController = mock[HealthController]

  val dataSource = mock[javax.sql.DataSource]
  val draftRepository = mock[ArticleRepository]
  val conceptRepository = mock[ConceptRepository]

  val converterService = mock[ConverterService]

  val readService = mock[ReadService]
  val writeService = mock[WriteService]
  val contentValidator = mock[ContentValidator]
  val importValidator = mock[ContentValidator]

  val ndlaClient = mock[NdlaClient]
  val searchConverterService = mock[SearchConverterService]
  val jestClient = mock[NdlaJestClient]

  val clock = mock[SystemClock]
  val authUser = mock[AuthUser]
  val authRole = new AuthRole

  val ArticleApiClient = mock[ArticleApiClient]
}
