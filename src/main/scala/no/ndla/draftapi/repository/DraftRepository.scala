/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties
import no.ndla.draftapi.integration.DataSource
import no.ndla.draftapi.model.api.{NotFoundException, OptimisticLockException}
import no.ndla.draftapi.model.domain._
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.write
import org.postgresql.util.PGobject
import scalikejdbc._

import scala.util.{Failure, Success, Try}

trait DraftRepository {
  this: DataSource =>
  val draftRepository: ArticleRepository

  class ArticleRepository extends LazyLogging with Repository[Article] {
    implicit val formats = org.json4s.DefaultFormats + Article.JSonSerializer + new EnumNameSerializer(ArticleStatus) + new EnumNameSerializer(
      ArticleType)

    def insert(article: Article)(implicit session: DBSession = AutoSession): Article = {
      val startRevision = article.revision.getOrElse(1)
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      sql"""
            insert into ${Article.table} (document, revision, article_id)
            values ($dataObject, $startRevision, ${article.id})
          """.updateAndReturnGeneratedKey().apply

      logger.info(s"Inserted new article: ${article.id}, with revision $startRevision")
      article.copy(revision = Some(startRevision))
    }

    def insertWithExternalIds(article: Article,
                              externalIds: List[String],
                              externalSubjectIds: Seq[String],
                              importId: Option[String])(implicit session: DBSession = AutoSession): Article = {
      val startRevision = 1
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      val uuid = Try(importId.map(UUID.fromString)).toOption.flatten

      val articleId: Long =
        sql"""
             insert into ${Article.table} (external_id, external_subject_id, document, revision, import_id, article_id)
             values (ARRAY[${externalIds}]::text[],
                     ARRAY[${externalSubjectIds}]::text[],
                     ${dataObject},
                     $startRevision,
                     $uuid,
                     ${article.id})
          """.updateAndReturnGeneratedKey().apply

      logger.info(s"Inserted new article: $articleId")
      article.copy(revision = Some(startRevision))
    }

    def newEmptyArticle(id: Long, externalIds: List[String], externalSubjectIds: Seq[String])(
        implicit session: DBSession = AutoSession): Try[Long] = {
      Try(sql"""
             insert into ${Article.table} (external_id, external_subject_id, article_id)
             values (ARRAY[${externalIds}]::text[], ARRAY[${externalSubjectIds}]::text[], $id)
          """.update.apply) match {
        case Success(_) =>
          logger.info(s"Inserted new empty article: $id")
          Success(id)
        case Failure(ex) => Failure(ex)
      }
    }

    def newArticleId()(implicit session: DBSession = AutoSession): Try[Long] = {
      Try(
        sql"""select article_id from ${Article.table} ORDER BY article_id DESC LIMIT 1"""
          .map(rs => rs.long("article_id"))
          .single()
          .apply()
      ) match {
        case Success(Some(id)) => Success(1 + id)
        case Success(None)     => Success(1)
        case Failure(ex)       => Failure(ex)
      }
    }

    def updateArticle(article: Article, isImported: Boolean = false)(
        implicit session: DBSession = AutoSession): Try[Article] = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      val newRevision = if (isImported) 1 else article.revision.getOrElse(0) + 1
      val oldRevision = if (isImported) 1 else article.revision
      val count =
        sql"""
              update ${Article.table}
              set document=$dataObject, revision=$newRevision
              where article_id=${article.id}
              and revision=$oldRevision
              and revision=(select max(revision) from ${Article.table} where article_id=${article.id})
           """.update.apply

      if (count != 1) {
        val message = s"Found revision mismatch when attempting to update article ${article.id}"
        logger.info(message)
        Failure(new OptimisticLockException)
      } else {
        logger.info(s"Updated article ${article.id}")
        Success(article.copy(revision = Some(newRevision)))
      }
    }

    def updateWithExternalIds(article: Article,
                              externalIds: List[String],
                              externalSubjectIds: Seq[String],
                              importId: Option[String])(implicit session: DBSession = AutoSession): Try[Article] = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      val uuid = Try(importId.map(UUID.fromString)).toOption.flatten
      val newRevision = article.revision.getOrElse(0) + 1

      val a = Article.syntax("ar")
      val b = Article.syntax("arb")

      val deletedCount = withSQL {
        delete
          .from(Article as a)
          .where
          .eq(a.c("article_id"), article.id)
          .and
          .notIn(a.id,
                 select(b.id)
                   .from(Article as b)
                   .where
                   .eq(b.c("article_id"), article.id)
                   .orderBy(b.revision)
                   .desc
                   .limit(1))
      }.update.apply
      logger.info(s"Deleted $deletedCount revisions of article with id '${article.id}' before import update.")

      val count = withSQL {
        update(Article as a)
          .set(
            sqls"""
                 document=$dataObject,
                 revision=1,
                 external_id=ARRAY[$externalIds]::text[],
                 external_subject_id=ARRAY[$externalSubjectIds]::text[],
                 import_id=$uuid
              """
          )
          .where
          .eq(a.c("article_id"), article.id)
      }.update.apply

      if (count != 1) {
        val message = s"Found revision mismatch when attempting to update article ${article.id}"
        logger.info(message)
        Failure(new OptimisticLockException)
      } else {
        logger.info(s"Updated article ${article.id}")
        Success(article.copy(revision = Some(newRevision)))
      }
    }

    def withId(articleId: Long): Option[Article] =
      articleWhere(
        sqls"ar.article_id=${articleId.toInt} AND ar.document#>>'{status,current}' <> ${ArticleStatus.ARCHIVED.toString} ORDER BY revision DESC LIMIT 1")

    def idsWithStatus(status: ArticleStatus.Value)(implicit session: DBSession = AutoSession): Try[List[ArticleIds]] = {
      val ar = Article.syntax("ar")
      Try(
        sql"select article_id, external_id from ${Article
          .as(ar)} where ar.document is not NULL and ar.document#>>'{status,current}' = ${status.toString}"
          .map(rs => ArticleIds(rs.long("article_id"), externalIdsFromResultSet(rs)))
          .list
          .apply)
    }

    def exists(id: Long)(implicit session: DBSession = AutoSession): Boolean = {
      sql"select article_id from ${Article.table} where article_id=$id order by revision desc limit 1"
        .map(rs => rs.long("article_id"))
        .single
        .apply()
        .isDefined
    }

    def deleteArticle(articleId: Long)(implicit session: DBSession = AutoSession): Try[Long] = {
      val numRows = sql"delete from ${Article.table} where article_id = $articleId".update().apply
      if (numRows == 1) {
        Success(articleId)
      } else {
        Failure(NotFoundException(s"Article with id $articleId does not exist"))
      }
    }

    def getIdFromExternalId(externalId: String)(implicit session: DBSession = AutoSession): Option[Long] = {
      sql"select article_id from ${Article.table} where ${externalId} = any (external_id) order by revision desc limit 1"
        .map(rs => rs.long("article_id"))
        .single
        .apply()
    }

    private def externalIdsFromResultSet(wrappedResultSet: WrappedResultSet): List[String] = {
      Option(wrappedResultSet.array("external_id"))
        .map(_.getArray.asInstanceOf[Array[String]])
        .getOrElse(Array.empty)
        .toList
        .flatMap(Option(_))
    }

    def getExternalIdsFromId(id: Long)(implicit session: DBSession = AutoSession): List[String] = {
      sql"select external_id from ${Article.table} where article_id=${id.toInt} order by revision desc limit 1"
        .map(externalIdsFromResultSet)
        .single
        .apply()
        .getOrElse(List.empty)
    }

    def getAllIds(implicit session: DBSession = AutoSession): Seq[ArticleIds] = {
      sql"select article_id, max(external_id) as external_id from ${Article.table} group by article_id order by article_id asc"
        .map(
          rs =>
            ArticleIds(
              rs.long("article_id"),
              externalIdsFromResultSet(rs)
          ))
        .list
        .apply
    }

    def articleCount(implicit session: DBSession = AutoSession): Long = {
      sql"select count(distinct article_id) from ${Article.table} where document is not NULL"
        .map(rs => rs.long("count"))
        .single()
        .apply()
        .getOrElse(0)
    }

    def getArticlesByPage(pageSize: Int, offset: Int)(implicit session: DBSession = AutoSession): Seq[Article] = {
      val ar = Article.syntax("ar")
      sql"""
           select *
           from (select
                   ${ar.result.*},
                   ${ar.revision} as revision,
                   max(revision) over (partition by article_id) as max_revision
                 from ${Article.as(ar)}
                 where document is not NULL) _
           where revision = max_revision
           offset $offset
           limit $pageSize
      """
        .map(Article(ar))
        .list
        .apply()
    }

    def allTags(implicit session: DBSession = AutoSession): Seq[ArticleTag] = {
      val allTags = sql"""select document->>'tags' from ${Article.table} where document is not NULL"""
        .map(rs => rs.string(1))
        .list
        .apply

      allTags
        .flatMap(tag => parse(tag).extract[List[ArticleTag]])
        .groupBy(_.language)
        .map {
          case (language, tags) =>
            ArticleTag(tags.flatMap(_.tags), language)
        }
        .toList
    }

    override def minMaxId(implicit session: DBSession = AutoSession): (Long, Long) = {
      sql"select coalesce(MIN(id),0) as mi, coalesce(MAX(id),0) as ma from ${Article.table}"
        .map(rs => {
          (rs.long("mi"), rs.long("ma"))
        })
        .single()
        .apply() match {
        case Some(minmax) => minmax
        case None         => (0L, 0L)
      }
    }

    override def documentsWithIdBetween(min: Long, max: Long): List[Article] =
      articlesWhere(
        sqls"ar.id between $min and $max and ar.document#>>'{status,current}' <> ${ArticleStatus.ARCHIVED.toString}").toList

    private def articleWhere(whereClause: SQLSyntax)(
        implicit session: DBSession = ReadOnlyAutoSession): Option[Article] = {
      val ar = Article.syntax("ar")
      sql"select ${ar.result.*} from ${Article.as(ar)} where ar.document is not NULL and $whereClause"
        .map(Article(ar))
        .single
        .apply()
    }

    private def articlesWhere(whereClause: SQLSyntax)(
        implicit session: DBSession = ReadOnlyAutoSession): Seq[Article] = {
      val ar = Article.syntax("ar")
      sql"select ${ar.result.*} from ${Article.as(ar)} where ar.document is not NULL and $whereClause"
        .map(Article(ar))
        .list
        .apply()
    }

    def importIdOfArticle(externalId: String)(implicit session: DBSession = ReadOnlyAutoSession): Option[ImportId] = {
      val ar = Article.syntax("ar")
      sql"""select ${ar.result.*}, import_id, external_id
            from ${Article.as(ar)}
            where ar.document is not NULL and $externalId = any (ar.external_id)"""
        .map(rs => ImportId(rs.stringOpt("import_id")))
        .single
        .apply()
    }

  }
}
