package no.ndla.draftapi.repository

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.integration.DataSource
import no.ndla.draftapi.model.domain.UserData
import org.json4s.Formats
import org.json4s.native.Serialization.write
import org.postgresql.util.PGobject
import scalikejdbc.interpolation.SQLSyntax
import scalikejdbc._

import scala.util.{Success, Try}

trait UserDataRepository {
  this: DataSource =>
  val userDataRepository: UserDataRepository

  class UserDataRepository extends LazyLogging with Repository[UserData]{ // TODO lage ny repository for UserData
    implicit val formats: Formats = org.json4s.DefaultFormats + UserData.JSonSerializer // TODO litt usikker på om disse er rett, gjorde som i AgreementRepository

    def insert(userData: UserData)(implicit session: DBSession = AutoSession): UserData = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(userData))

      val userDataId = // TODO skriv sql
        sql"""
        insert into ${UserData.table} ...
        """.updateAndReturnGeneratedKey().apply

      logger.info(s"Inserted new user data: $userDataId")
      userData.copy(id = Some(userDataId)) // todo hva er id?
    }

    def withId(userId: String): Option[UserData] =
      agreementWhere(sqls"agr.id=${userId.toInt}")

    def updateSavedSearches(userData: UserData)(implicit  session: DBSession = AutoSession): Try[UserData] = {
      val dataObject = new PGobject()
      dataObject.setType("jsonb")
      dataObject.setValue(write(userData))

      val count = // TODO skriv sql
        sql"""
            update ${UserData.table}
            set document=$dataObject
            where user_id=${userData.id}
        """.update.apply

      logger.info(s"Updated user data ${userData.id} / ${userData}")
      Success(userData)
    }
  }

  private def agreementWhere(whereClause: SQLSyntax)(
      implicit session: DBSession = ReadOnlyAutoSession): Option[UserData] = {
    val userdata = UserData.syntax("userdata") // TODO skriv sql (brukt AgreementRepository, linje 72 som utgangspunkt)
    sql"select ${userdata.result.*} from "

  }

}
