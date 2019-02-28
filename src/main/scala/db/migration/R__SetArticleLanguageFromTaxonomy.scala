/*
 * Part of NDLA draft-api.
 * Copyright (C) 2019 NDLA
 *
 * See LICENSE
 */

package db.migration

import no.ndla.draftapi.DraftApiProperties.Domain
import no.ndla.draftapi.model.domain._
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}
import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JArray, JField, JObject, JString}
import org.json4s.native.JsonMethods.{compact, parse, render}
import org.json4s.native.Serialization.write
import org.postgresql.util.PGobject
import scalaj.http.Http
import scalikejdbc.{DB, DBSession, _}
import no.ndla.mapping.ISO639.get6391CodeFor6392Code

import scala.util.Try
import scala.util.matching.Regex

class R__SetArticleLanguageFromTaxonomy extends BaseJavaMigration {

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  private val TaxonomyApiEndpoint = s"$Domain/taxonomy/v1"
  private val taxonomyTimeout = 20 * 1000 // 20 Seconds

  case class TaxonomyResource(contentUri: Option[String])
  case class Keyword(names: List[KeywordName])
  case class KeywordName(data: List[Map[String, String]])

  override def getChecksum: Integer = 2 // Change this to something else if you want to repeat migration

  def fetchResourceFromTaxonomy(endpoint: String): Seq[Long] = {
    val url = TaxonomyApiEndpoint + endpoint

    val resourceList = for {
      response <- Try(Http(url).asString)
      extracted <- Try(parse(response.body).extract[Seq[TaxonomyResource]])
    } yield extracted

    resourceList
      .getOrElse(Seq.empty)
      .flatMap(resource =>
        resource.contentUri.flatMap(contentUri => {
          val splits = contentUri.split(':')
          val articleId = splits.lastOption.filter(_ => splits.contains("article"))
          articleId.flatMap(idStr => Try(idStr.toLong).toOption)
        }))
  }

  def fetchArticleTags(externalId: Long): Seq[ArticleTag] = {

    val url = "http://api.topic.ndla.no/rest/v1/keywords/?filter%5Bnode%5D=ndlanode_" + externalId.toString

    val keywords = for {
      response <- Try(Http(url).asString)
      extracted <- Try(parse(response.body).extract[Seq[Keyword]])
    } yield extracted

    keywords
      .map(
        _.flatMap(_.names)
          .flatMap(_.data)
          .flatMap(_.toIterable)
          .map(t => (getISO639(t._1), t._2.trim.toLowerCase))
          .groupBy(_._1)
          .map(entry => (entry._1, entry._2.map(_._2)))
          .map(t => ArticleTag(t._2, Language.languageOrUnknown(t._1)))
          .toList)
      .getOrElse(Seq())

    //Seq()

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

    val topicIds: Seq[Long] = fetchResourceFromTaxonomy("/subjects/urn:subject:15/topics?recursive=true")
    val resourceIds: Seq[Long] = fetchResourceFromTaxonomy("subjects/urn:subject:15/resources")
    val tags: Seq[ArticleTag] = topicIds.flatMap(fetchArticleTags)
    System.out.println("length topic: " + topicIds.length)
    System.out.println("length resource: " + resourceIds.length)
    System.out.println("tagg: " + tags.length)
    val k = for {
      topicId <- topicIds
      article <- fetchArticleInfo(topicId)

    } yield convertArticleLanguage(article, Seq())

    k.map(updateArticle)

    val abc = for {
      resourceId <- resourceIds
      article <- fetchArticleInfo(resourceId)
    } yield convertArticleLanguage(article, Seq())

    abc.map(updateArticle)

  }

  def fetchArticleInfo(articleId: Long)(implicit session: DBSession): Option[Article] = {
    val ar = Article.syntax("ar")
    val withId =
      sqls"ar.article_id=${articleId.toInt} AND ar.document#>>'{status,current}' <> ${ArticleStatus.ARCHIVED.toString} ORDER BY revision DESC LIMIT 1"
    sql"select ${ar.result.*} from ${Article.as(ar)} where ar.document is not NULL and $withId"
      .map(Article(ar))
      .single()
      .apply()
  }

  def convertArticleLanguage(oldArticle: Article, newTags: Seq[ArticleTag]): Article = {
    oldArticle.copy(
      title = oldArticle.title.map(copyArticleTitle),
      content = oldArticle.content.map(copyArticleContent),
      //tags = newTags,
      visualElement = oldArticle.visualElement.map(copyVisualElement),
      introduction = oldArticle.introduction.map(copyArticleIntroduction),
      metaDescription = oldArticle.metaDescription.map(copyArticleMetaDescription),
      metaImage = oldArticle.metaImage.map(copyArticleMetaImage)
    )
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

    System.out.println("Saving article with id: " + article.id)

    sql"update articledata set document = $dataObject where article_id=${article.id}"
      .update()
      .apply
  }

}
