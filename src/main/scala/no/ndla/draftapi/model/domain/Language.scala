/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.model.domain

import com.sksamuel.elastic4s.analyzers._
import no.ndla.mapping.ISO639

object Language {
  val DefaultLanguage = "nb"
  val UnknownLanguage = "unknown"
  val NoLanguage = ""
  val AllLanguages = "all"

  val languageAnalyzers = Seq(
    LanguageAnalyzer(DefaultLanguage, NorwegianLanguageAnalyzer),
    LanguageAnalyzer("nn", NorwegianLanguageAnalyzer),
    LanguageAnalyzer("en", EnglishLanguageAnalyzer),
    LanguageAnalyzer("fr", FrenchLanguageAnalyzer),
    LanguageAnalyzer("de", GermanLanguageAnalyzer),
    LanguageAnalyzer("es", SpanishLanguageAnalyzer),
    LanguageAnalyzer("se", StandardAnalyzer), // SAMI
    LanguageAnalyzer("zh", ChineseLanguageAnalyzer),
    LanguageAnalyzer(UnknownLanguage, StandardAnalyzer)
  )

  val supportedLanguages = languageAnalyzers.map(_.lang)

  def findByLanguageOrBestEffort[P <: LanguageField[_]](sequence: Seq[P], language: String): Option[P] = {
    sequence.find(_.language == language).orElse(
      sequence.sortBy(lf =>
        ISO639.languagePriority.reverse.indexOf(lf.language)).lastOption)
  }

  def getSupportedLanguages(sequences: Seq[Seq[WithLanguage]]): Seq[String] = {
    sequences.flatMap(_.map(_.language)).distinct
  }

  def getSearchLanguage(languageParam: String, supportedLanguages: Seq[String]): String = {
    val l = if (languageParam == AllLanguages) DefaultLanguage else languageParam
    if (supportedLanguages.contains(l))
      l
    else
      supportedLanguages.head
  }

  def findByLanguage[T](sequence: Seq[LanguageField[T]], lang: String): Option[LanguageField[T]] = {
    sequence.find(_.language == lang)
  }

  def findValueByLanguage[T](sequence: Seq[LanguageField[T]], lang: String): Option[T] = {
    findByLanguage(sequence, lang).map(_.value)
  }
}

case class LanguageAnalyzer(lang: String, analyzer: Analyzer)
