/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service.search

import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.indexes.IndexDefinition
import com.sksamuel.elastic4s.mappings.MappingDefinition
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.model.domain.Agreement
import no.ndla.draftapi.model.search.{SearchableArticle, SearchableLanguageFormats}
import no.ndla.draftapi.repository.{AgreementRepository, Repository}
import org.json4s.native.Serialization.write

trait AgreementIndexService {
  this: SearchConverterService with IndexService with AgreementRepository =>
  val agreementIndexService: AgreementIndexService

  class AgreementIndexService extends LazyLogging with IndexService[Agreement, SearchableArticle] {
    implicit val formats = SearchableLanguageFormats.JSonFormats
    override val documentType: String = DraftApiProperties.AgreementSearchDocument
    override val searchIndex: String = DraftApiProperties.AgreementSearchIndex
    override val repository: Repository[Agreement] = agreementRepository

    override def createIndexRequest(domainModel: Agreement, indexName: String): IndexDefinition = {
      val source = write(searchConverterService.asSearchableAgreement(domainModel))
      indexInto(indexName / documentType).doc(source).id(domainModel.id.get.toString)
    }

    def getMapping: MappingDefinition = {
      mapping(documentType).fields(
        intField("id"),
        textField("title").fielddata(true).fields(keywordField("raw")),
        textField("content").fielddata(true),
        keywordField("license")
      )
    }
  }
}
