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
import org.scalatest.mockito.MockitoSugar

trait TestEnvironment
  extends ElasticClient
    with Elastic4sClient
    with ArticleSearchService
    with ArticleIndexService
    with ConceptSearchService
    with ConceptIndexService
    with AgreementSearchService
    with AgreementIndexService
    with IndexService
    with SearchService
    with LazyLogging
    with DraftController
    with InternController
    with HealthController
    with ConceptController
    with AgreementController
    with ReindexClient
    with DataSource
    with DraftRepository
    with AgreementRepository
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
  val agreementSearchService = mock[AgreementSearchService]
  val agreementIndexService = mock[AgreementIndexService]

  val internController = mock[InternController]
  val draftController = mock[DraftController]
  val conceptController = mock[ConceptController]
  val agreementController = mock[AgreementController]

  val healthController = mock[HealthController]

  val dataSource = mock[javax.sql.DataSource]
  val draftRepository = mock[ArticleRepository]
  val conceptRepository = mock[ConceptRepository]
  val agreementRepository = mock[AgreementRepository]

  val converterService = mock[ConverterService]

  val readService = mock[ReadService]
  val writeService = mock[WriteService]
  val contentValidator = mock[ContentValidator]
  val importValidator = mock[ContentValidator]
  val reindexClient = mock[ReindexClient]

  val ndlaClient = mock[NdlaClient]
  val searchConverterService = mock[SearchConverterService]
  val jestClient = mock[NdlaJestClient]
  val e4sClient = mock[NdlaE4sClient]

  val clock = mock[SystemClock]
  val authUser = mock[AuthUser]
  val authRole = new AuthRole

  val ArticleApiClient = mock[ArticleApiClient]
}
