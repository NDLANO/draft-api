/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service.search

import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.indexes.IndexDefinition
import com.sksamuel.elastic4s.mappings.{MappingDefinition, NestedFieldDefinition}
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.model.domain.Concept
import no.ndla.draftapi.model.domain.Language.languageAnalyzers
import no.ndla.draftapi.model.search.{SearchableArticle, SearchableLanguageFormats}
import no.ndla.draftapi.repository.{ConceptRepository, Repository}
import org.json4s.native.Serialization.write

trait ConceptIndexService {
  this: IndexService with ConceptRepository with SearchConverterService =>
  val conceptIndexService: ConceptIndexService

  class ConceptIndexService extends LazyLogging with IndexService[Concept, SearchableArticle] {
    implicit val formats = SearchableLanguageFormats.JSonFormats
    override val documentType: String = DraftApiProperties.ConceptSearchDocument
    override val searchIndex: String = DraftApiProperties.ConceptSearchIndex
    override val repository: Repository[Concept] = conceptRepository

    override def createIndexRequest(concept: Concept, indexName: String): IndexDefinition = {
      val source = write(searchConverterService.asSearchableConcept(concept))
      indexInto(indexName / documentType).doc(source).id(concept.id.get.toString)
    }

    def getMapping: MappingDefinition = {
      mapping(documentType).fields(
        List(
          intField("id"),
          keywordField("defaultTitle")
        ) ++
          generateLanguageSupportedFieldList("title", keepRaw = true) ++
          generateLanguageSupportedFieldList("content")
      )
    }

  }

}
