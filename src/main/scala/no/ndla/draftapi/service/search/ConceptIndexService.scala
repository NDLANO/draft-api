/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi.service.search

import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.mappings.{MappingContentBuilder, NestedFieldDefinition}
import com.typesafe.scalalogging.LazyLogging
import io.searchbox.core.Index
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.model.domain.{Article, Concept}
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

    override def createIndexRequest(concept: Concept, indexName: String): Index = {
      val source = write(searchConverterService.asSearchableConcept(concept))
      new Index.Builder(source).index(indexName).`type`(documentType).id(concept.id.get.toString).build
    }

    def getMapping: String = {
      MappingContentBuilder.buildWithName(mapping(documentType).fields(
        intField("id"),
        languageSupportedField("title", keepRaw = true),
        languageSupportedField("content")
      ), DraftApiProperties.ConceptSearchDocument).string()
    }

    private def languageSupportedField(fieldName: String, keepRaw: Boolean = false) = {
      val languageSupportedField = new NestedFieldDefinition(fieldName)
      languageSupportedField._fields = keepRaw match {
        case true => languageAnalyzers.map(langAnalyzer => textField(langAnalyzer.lang).fielddata(true) analyzer langAnalyzer.analyzer fields (keywordField("raw") index "not_analyzed"))
        case false => languageAnalyzers.map(langAnalyzer => textField(langAnalyzer.lang).fielddata(true) analyzer langAnalyzer.analyzer)
      }

      languageSupportedField
    }

  }
}
