package db.migration
import no.ndla.draftapi.model.domain.{Article, ArticleStatus, ArticleType}
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}
import org.json4s.ext.EnumNameSerializer
import scalikejdbc.{DB, DBSession, _}

class R__RemoveStatusPublishedArticles extends BaseJavaMigration {

  implicit val formats = org.json4s.DefaultFormats + new EnumNameSerializer(ArticleStatus) + new EnumNameSerializer(
    ArticleType)

  override def getChecksum: Integer = 0 // Change this to something else if you want to repeat migration

  override def migrate(context: Context): Unit = {
    val db = DB(context.getConnection)
    db.autoClose(false)

    db.withinTx { implicit session =>
      migrateArticles
    }
  }

  def migrateArticles(implicit session: DBSession): Unit = {
    val count = countAllArticles.get
    var numPagesLeft = (count / 1000) + 1
    var offset = 0L
    while (numPagesLeft > 0) {
      allArticles(offset * 1000).map(updateStatus)
      numPagesLeft -= 1
      offset += 1
    }

  }

  def countAllArticles(implicit session: DBSession): Option[Long] = {
    sql"select count(*) from articledata where document is not NULL"
      .map(rs => rs.long("count"))
      .single()
      .apply()
  }

  def allArticles(offset: Long)(implicit session: DBSession): Seq[Article] = {
    val ar = Article.syntax("ar")
    sql"select ${ar.result.*} from ${Article.as(ar)} where ar.document is not NULL order by ar.id limit 1000 offset $offset"
      .map(Article(ar))
      .list
      .apply()
  }

  def updateStatus(article: Article) = {

  }

}
