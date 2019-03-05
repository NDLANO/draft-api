/*
 * Part of NDLA draft-api.
 * Copyright (C) 2019 NDLA
 *
 * See LICENSE
 */

package db.migration

import no.ndla.draftapi.DraftApiProperties.Domain
import no.ndla.draftapi.model.domain._
import no.ndla.mapping.ISO639.get6391CodeFor6392Code
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization.write
import org.postgresql.util.PGobject
import scalaj.http.Http
import scalikejdbc.{DB, DBSession, _}

import scala.util.matching.Regex
import scala.util.{Failure, Random, Success, Try}

class R__SetArticleLanguageFromTaxonomy extends BaseJavaMigration {

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  private val TaxonomyApiEndpoint = s"$Domain/taxonomy/v1"
  private val taxonomyTimeout = 20 * 1000 // 20 Seconds

  case class TaxonomyResource(contentUri: Option[String], id: Option[String])

  case class Keywords(keyword: List[Keyword])
  case class Keyword(names: List[KeywordName])
  case class KeywordName(data: List[Map[String, String]])

  override def getChecksum: Integer = Random.nextInt() // Change this to something else if you want to repeat migration

  def fetchResourceFromTaxonomy(endpoint: String): Seq[(Long, Long)] = {
    val url = TaxonomyApiEndpoint + endpoint

    val resourceList = for {
      response <- Try(Http(url).asString)
      extracted <- Try(parse(response.body).extract[Seq[TaxonomyResource]])
    } yield extracted

    resourceList.getOrElse(Seq.empty).flatMap(trim)
  }

  def trim(resource: TaxonomyResource): Option[(Long, Long)] = {

    (resource.contentUri, resource.id) match {
      case (Some(uri), Some(id)) => Some(uri.split(':').last.toLong, id.split(':').last.toLong)
      case _                     => None
    }

  }

  def fetchArticleTags(externalId: Long): Seq[ArticleTag] = {

    val url = "http://api.topic.ndla.no/rest/v1/keywords/?filter%5Bnode%5D=ndlanode_" + externalId.toString

    val keywordsT = for {
      response <- Try(Http(url).asString)
      extracted <- Try(parse(response.body).extract[Keywords])
    } yield extracted

    keywordsT match {
      case Failure(_) => Seq()
      case Success(keywords) =>
        keywords.keyword
          .flatMap(_.names)
          .flatMap(_.data)
          .flatMap(_.toIterable)
          .map(t => (getISO639(t._1), t._2.trim.toLowerCase))
          .groupBy(_._1)
          .map(entry => (entry._1, entry._2.map(_._2)))
          .map(t => ArticleTag(t._2, Language.languageOrUnknown(t._1)))
          .toList
    }

  }

  def getISO639(languageUrl: String): Option[String] = {
    val pattern = new Regex("http:\\/\\/psi\\..*\\/#(.+)")
    Option(languageUrl) collect { case pattern(group) => group } match {
      case Some(x) =>
        if (x == "language-neutral") None else get6391CodeFor6392Code(x)
      case None => None
    }
  }

  override def migrate(context: Context): Unit = {
    val db = DB(context.getConnection)
    db.autoClose(false)

    db.withinTx { implicit session =>
      migrateArticles
    }
  }

  def migrateArticles(implicit session: DBSession): Unit = {

    val topicIdsList: Seq[(Long, Long)] = fetchResourceFromTaxonomy("/subjects/urn:subject:15/topics?recursive=true")
    val convertedTopicArticles = topicIdsList.map(topicIds => convertArticle(topicIds._1, topicIds._2))

    for {
      convertedArticle <- convertedTopicArticles
      article <- convertedArticle
    } yield updateArticle(article)

    val resourceIdsList: Seq[(Long, Long)] = fetchResourceFromTaxonomy("subjects/urn:subject:15/resources")
    val convertedResourceArticles = resourceIdsList.map(topicIds => convertArticle(topicIds._1, topicIds._2))

    for {
      convertedArticle <- convertedResourceArticles
      article <- convertedArticle
    } yield updateArticle(article)

  }

  def convertArticle(articleId: Long, externalId: Long)(implicit session: DBSession): Option[Article] = {
    val externalTags = fetchArticleTags(externalId)
    val oldArticle = fetchArticleInfo(articleId)
    convertArticleLanguage(oldArticle, externalTags)
  }

  def fetchArticleInfo(articleId: Long)(implicit session: DBSession): Option[Article] = {
    val ar = Article.syntax("ar")
    val withId =
      sqls"ar.id=${articleId.toInt} ORDER BY revision DESC LIMIT 1"
    sql"select ${ar.result.*} from ${Article.as(ar)} where ar.document is not NULL and $withId"
      .map(Article(ar))
      .single()
      .apply()
  }

  def convertArticleLanguage(oldArticle: Option[Article], externalTags: Seq[ArticleTag]): Option[Article] = {
    oldArticle.map(
      article =>
        article.copy(
          title = article.title.map(copyArticleTitle),
          content = article.content.map(copyArticleContent),
          tags = mergeTags(article.tags, externalTags),
          visualElement = article.visualElement.map(copyVisualElement),
          introduction = article.introduction.map(copyArticleIntroduction),
          metaDescription = article.metaDescription.map(copyArticleMetaDescription),
          metaImage = article.metaImage.map(copyArticleMetaImage)
      ))
  }

  def mergeTags(oldTags: Seq[ArticleTag], externalTags: Seq[ArticleTag]): Seq[ArticleTag] = {
    val combinedSeq = oldTags ++ externalTags
    combinedSeq.groupBy(_.language).map(mapEntry => createTag(mapEntry._1, mapEntry._2)).toSeq
  }

  def createTag(language: String, tags: Seq[ArticleTag]): ArticleTag = {
    val distinctTags = tags.flatMap(_.tags).distinct
    ArticleTag(distinctTags, language)
  }

  def copyArticleTitle(field: ArticleTitle): ArticleTitle = {
    if (field.language == "unknown") field.copy(language = "sma") else field
  }

  def copyArticleContent(field: ArticleContent): ArticleContent = {
    if (field.language == "unknown") field.copy(language = "sma") else field
  }

  def copyVisualElement(field: VisualElement): VisualElement = {
    if (field.language == "unknown") field.copy(language = "sma") else field
  }

  def copyArticleIntroduction(field: ArticleIntroduction): ArticleIntroduction = {
    if (field.language == "unknown") field.copy(language = "sma") else field
  }

  def copyArticleMetaDescription(field: ArticleMetaDescription): ArticleMetaDescription = {
    if (field.language == "unknown") field.copy(language = "sma") else field
  }

  def copyArticleMetaImage(field: ArticleMetaImage): ArticleMetaImage = {
    if (field.language == "unknown") field.copy(language = "sma") else field
  }

  def updateArticle(article: Article)(implicit session: DBSession): Long = {
    val dataObject = new PGobject()
    dataObject.setType("jsonb")
    dataObject.setValue(write(article))

    sql"update articledata set document = $dataObject where article_id=${article.id}"
      .update()
      .apply
  }

}