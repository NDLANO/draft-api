/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

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
      val startRevision = 1
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      val articleId: Long =
        sql"insert into ${Article.table} (id, document, revision) values (${article.id}, ${dataObject}, $startRevision)"
          .updateAndReturnGeneratedKey()
          .apply

      logger.info(s"Inserted new article: $articleId")
      article.copy(revision = Some(startRevision))
    }

    def insertWithExternalIds(article: Article, externalIds: List[String], externalSubjectIds: Seq[String])(
        implicit session: DBSession = AutoSession): Article = {
      val startRevision = 1
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      val articleId: Long =
        sql"""
             insert into ${Article.table} (id, external_id, external_subject_id, document, revision)
             values (${article.id}, ARRAY[${externalIds}]::text[], ARRAY[${externalSubjectIds}]::text[], ${dataObject}, $startRevision)
          """.updateAndReturnGeneratedKey().apply

      logger.info(s"Inserted new article: $articleId")
      article.copy(revision = Some(startRevision))
    }

    def newEmptyArticle(id: Long, externalIds: List[String], externalSubjectIds: Seq[String])(
        implicit session: DBSession = AutoSession): Try[Long] = {
      Try(sql"""
             insert into ${Article.table} (id, external_id, external_subject_id)
             values (${id}, ARRAY[${externalIds}]::text[], ARRAY[${externalSubjectIds}]::text[])
          """.update.apply) match {
        case Success(_) =>
          logger.info(s"Inserted new empty article: $id")
          Success(id)
        case Failure(ex) => Failure(ex)
      }
    }

    def update(article: Article, isImported: Boolean = false)(
        implicit session: DBSession = AutoSession): Try[Article] = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      val newRevision = if (isImported) 1 else article.revision.getOrElse(0) + 1
      val oldRevision = if (isImported) 1 else article.revision
      val count =
        sql"update ${Article.table} set document=${dataObject}, revision=$newRevision where id=${article.id} and revision=${oldRevision}".update.apply

      if (count != 1) {
        val message = s"Found revision mismatch when attempting to update article ${article.id}"
        logger.info(message)
        Failure(new OptimisticLockException)
      } else {
        logger.info(s"Updated article ${article.id}")
        Success(article.copy(revision = Some(newRevision)))
      }
    }

    def updateWithExternalIds(article: Article, externalIds: List[String], externalSubjectIds: Seq[String])(
        implicit session: DBSession = AutoSession): Try[Article] = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(article))

      val newRevision = article.revision.getOrElse(0) + 1
      val count =
        sql"update ${Article.table} set document=${dataObject}, revision=1, external_id=ARRAY[$externalIds]::text[], external_subject_id=ARRAY[${externalSubjectIds}]::text[] where id=${article.id}".update.apply

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
        sqls"ar.id=${articleId.toInt} AND ar.document#>>'{status,current}' <> ${ArticleStatus.ARCHIEVED.toString}")

    def idsWithStatus(status: ArticleStatus.Value)(implicit session: DBSession = AutoSession): Try[List[Long]] = {
      val ar = Article.syntax("ar")
      Try(
        sql"select ${ar.result.*} from ${Article.as(ar)} where ar.document is not NULL and ar.document#>>'{status,current}' = $status"
          .map(rs => rs.long("id"))
          .list
          .apply)
    }

    def exists(id: Long)(implicit session: DBSession = AutoSession): Boolean = {
      sql"select id from ${Article.table} where id=${id}".map(rs => rs.long("id")).single.apply().isDefined
    }

    def delete(articleId: Long)(implicit session: DBSession = AutoSession): Try[Long] = {
      val numRows = sql"delete from ${Article.table} where id = $articleId".update().apply
      if (numRows == 1) {
        Success(articleId)
      } else {
        Failure(NotFoundException(s"Article with id $articleId does not exist"))
      }
    }

    def getIdFromExternalId(externalId: String)(implicit session: DBSession = AutoSession): Option[Long] = {
      sql"select id from ${Article.table} where ${externalId} = any (external_id)"
        .map(rs => rs.long("id"))
        .single
        .apply()
    }

    private def externalIdsFromResultSet(wrappedResultSet: WrappedResultSet): List[String] = {
      Option(wrappedResultSet.array("external_id"))
        .map(_.getArray.asInstanceOf[Array[String]])
        .getOrElse(Array.empty)
        .toList
    }

    def getExternalIdsFromId(id: Long)(implicit session: DBSession = AutoSession): List[String] = {
      sql"select external_id from ${Article.table} where id=${id.toInt}"
        .map(externalIdsFromResultSet)
        .single
        .apply()
        .getOrElse(List.empty)
    }

    def getAllIds(implicit session: DBSession = AutoSession): Seq[ArticleIds] = {
      sql"select id, external_id from ${Article.table}"
        .map(
          rs =>
            ArticleIds(
              rs.long("id"),
              externalIdsFromResultSet(rs)
          ))
        .list
        .apply
    }

    def articleCount(implicit session: DBSession = AutoSession): Long = {
      sql"select count(*) from ${Article.table} where document is not NULL"
        .map(rs => rs.long("count"))
        .single()
        .apply()
        .getOrElse(0)
    }

    def getArticlesByPage(pageSize: Int, offset: Int)(implicit session: DBSession = AutoSession): Seq[Article] = {
      val ar = Article.syntax("ar")
      sql"select ${ar.result.*} from ${Article.as(ar)} where document is not NULL offset $offset limit $pageSize"
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
        sqls"ar.id between $min and $max and ar.document#>>'{status,current}' <> ${ArticleStatus.ARCHIEVED.toString}").toList

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

  }
}
