/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi

import com.typesafe.scalalogging.LazyLogging
import no.ndla.network.secrets.PropertyKeys
import no.ndla.network.secrets.Secrets.readSecrets
import no.ndla.network.{AuthUser, Domains}
import no.ndla.validation.ResourceType

import scala.util.Properties._
import scala.util.{Failure, Success}

object DraftApiProperties extends LazyLogging {
  val IsKubernetes: Boolean = envOrNone("NDLA_IS_KUBERNETES").isDefined

  val Environment = propOrElse("NDLA_ENVIRONMENT", "local")
  val ApplicationName = "draft-api"
  val Auth0LoginEndpoint = s"https://${AuthUser.getAuth0HostForEnv(Environment)}/authorize"
  val DraftRoleWithWriteAccess = "drafts:write"
  val DraftRoleWithPublishAccess = "drafts:set_to_publish"
  val ArticleRoleWithPublishAccess = "articles:publish"

  val ApplicationPort = propOrElse("APPLICATION_PORT", "80").toInt
  val ContactEmail = "christergundersen@ndla.no"

  lazy val MetaUserName = prop(PropertyKeys.MetaUserNameKey)
  lazy val MetaPassword = prop(PropertyKeys.MetaPasswordKey)
  lazy val MetaResource = prop(PropertyKeys.MetaResourceKey)
  lazy val MetaServer = prop(PropertyKeys.MetaServerKey)
  lazy val MetaPort = prop(PropertyKeys.MetaPortKey).toInt
  lazy val MetaSchema = prop(PropertyKeys.MetaSchemaKey)
  val MetaMaxConnections = 20

  val resourceHtmlEmbedTag = "embed"
  val ApiClientsCacheAgeInMs: Long = 1000 * 60 * 60 // 1 hour caching

  val externalApiUrls = Map(
    ResourceType.Image.toString -> s"$Domain/image-api/v2/images",
    "raw-image" -> s"$Domain/image-api/raw/id",
    ResourceType.Audio.toString -> s"$Domain/audio-api/v1/audio",
    ResourceType.File.toString -> Domain
  )

  val internalApiUrls = Map(
    "article-api" -> "http://article-api.ndla-local/intern",
    "audio-api" -> "http://audio-api.ndla-local/intern",
    "draft-api" -> "http://draft-api.ndla-local/intern",
    "image-api" -> "http://image-api.ndla-local/intern"
  )

  val NDLABrightcoveAccountId = prop("NDLA_BRIGHTCOVE_ACCOUNT_ID")
  val NDLABrightcovePlayerId = prop("NDLA_BRIGHTCOVE_PLAYER_ID")

  val NDLABrightcoveVideoScriptUrl =
    s"//players.brightcove.net/$NDLABrightcoveAccountId/${NDLABrightcovePlayerId}_default/index.min.js"
  val H5PResizerScriptUrl = "//ndla.no/sites/all/modules/h5p/library/js/h5p-resizer.js"
  val NRKVideoScriptUrl = Seq("//www.nrk.no/serum/latest/js/video_embed.js", "//nrk.no/serum/latest/js/video_embed.js")

  val SearchServer = propOrElse("SEARCH_SERVER", "http://search-draft-api.ndla-local")
  val SearchRegion = propOrElse("SEARCH_REGION", "eu-central-1")
  val RunWithSignedSearchRequests = propOrElse("RUN_WITH_SIGNED_SEARCH_REQUESTS", "true").toBoolean
  val DraftSearchIndex = propOrElse("SEARCH_INDEX_NAME", "draft-articles")
  val ConceptSearchIndex = propOrElse("CONCEPT_SEARCH_INDEX_NAME", "draft-concepts")
  val AgreementSearchIndex = propOrElse("AGREEMENT_SEARCH_INDEX_NAME", "draft-agreements")
  val DraftSearchDocument = "article-drafts"
  val AgreementSearchDocument = "agreement-drafts"
  val ConceptSearchDocument = "concept-drafts"
  val DefaultPageSize = 10
  val MaxPageSize = 100
  val IndexBulkSize = 200
  val ElasticSearchIndexMaxResultWindow = 10000
  val ElasticSearchScrollKeepAlive = "10s"

  val CorrelationIdKey = "correlationID"
  val CorrelationIdHeader = "X-Correlation-ID"

  val ArticleApiHost = propOrElse("ARTICLE_API_HOST", "article-api.ndla-local")
  val LearningpathApiHost = propOrElse("LEARNINGPATH_API_HOST", "learningpath-api.ndla-local")

  val AttachmentStorageName = s"$Environment.article-attachments.ndla"

  lazy val Domain = Domains.get(Environment)

  lazy val secrets = {
    val SecretsFile = "draft-api.secrets"
    readSecrets(SecretsFile) match {
      case Success(values) => values
      case Failure(exception) =>
        throw new RuntimeException(s"Unable to load remote secrets from $SecretsFile", exception)
    }
  }

  lazy val supportedUploadExtensions = Set(
    ".csv",
    ".doc",
    ".docx",
    ".ggb",
    ".json",
    ".odp",
    ".ods",
    ".odt",
    ".pdf",
    ".ppt",
    ".pptx",
    ".pub",
    ".rtf",
    ".tex",
    ".tsv",
    ".txt",
    ".xls",
    ".xlsx",
    ".xml"
  )

  def booleanProp(key: String): Boolean = prop(key).toBoolean

  def prop(key: String): String = {
    propOrElse(key, throw new RuntimeException(s"Unable to load property $key"))
  }

  def propOpt(key: String): Option[String] = {
    envOrNone(key) match {
      case Some(prop)            => Some(prop)
      case None if !IsKubernetes => secrets.get(key).flatten
      case _                     => None
    }
  }

  def propOrElse(key: String, default: => String): String = propOpt(key).getOrElse(default)
}
