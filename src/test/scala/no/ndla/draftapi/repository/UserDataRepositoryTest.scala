/*
 * Part of NDLA draft-api.
 * Copyright (C) 2020 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.repository

import java.net.Socket

import no.ndla.draftapi.{DBMigrator, DraftApiProperties, IntegrationSuite, TestData, TestEnvironment}
import org.postgresql.util.PSQLException
import scalikejdbc._

import scala.util.{Failure, Success, Try}

class UserDataRepositoryTest extends IntegrationSuite with TestEnvironment {
  var repository: UserDataRepository = _

  def emptyTestDatabase = {
    DB autoCommit (implicit session => {
      sql"delete from draftapitest.userdata;".execute.apply()(session)
    })
  }

  private def resetIdSequence() = {
    DB autoCommit (implicit session => {
      sql"select setval('userdata_id_seq', 1, false);".execute.apply
    })
  }

  def serverIsListening: Boolean = {
    Try(new Socket(DraftApiProperties.MetaServer, DraftApiProperties.MetaPort)) match {
      case Success(c) =>
        c.close()
        true
      case _ => false
    }
  }

  def databaseIsAvailable: Boolean = Try(repository.userDataCount).isSuccess

  override def beforeEach(): Unit = {
    repository = new UserDataRepository
    if (databaseIsAvailable) {
      emptyTestDatabase
    }
  }

  override def beforeAll(): Unit = {
    Try {
      val datasource = testDataSource.get
      if (serverIsListening) {
        ConnectionPool.singleton(new DataSourceConnectionPool(datasource))
        DBMigrator.migrate(datasource)
      }
    }
  }

  test("that inserting records to database is generating id as expected") {
    assume(databaseIsAvailable, "Database is unavailable")

    this.resetIdSequence()

    val data1 = TestData.emptyDomainUserData.copy(userId = "user1")
    val data2 = TestData.emptyDomainUserData.copy(userId = "user2")
    val data3 = TestData.emptyDomainUserData.copy(userId = "user3")

    val res1 = repository.insert(data1)
    val res2 = repository.insert(data2)
    val res3 = repository.insert(data3)

    res1.get.id should be(Some(1))
    res2.get.id should be(Some(2))
    res3.get.id should be(Some(3))
  }

  test("that withId and withUserId returns the same userdata") {
    assume(databaseIsAvailable, "Database is unavailable")

    this.resetIdSequence()

    val data1 = TestData.emptyDomainUserData.copy(userId = "first", savedSearches = Some(Seq("eple")))
    val data2 = TestData.emptyDomainUserData.copy(userId = "second", latestEditedArticles = Some(Seq("kake")))
    val data3 = TestData.emptyDomainUserData.copy(userId = "third", favoriteSubjects = Some(Seq("bok")))

    repository.insert(data1)
    repository.insert(data2)
    repository.insert(data3)

    repository.withId(1).get should be(repository.withUserId("first").get)
    repository.withId(2).get should be(repository.withUserId("second").get)
    repository.withId(3).get should be(repository.withUserId("third").get)
  }

  test("that updating updates all fields correctly") {
    assume(databaseIsAvailable, "Database is unavailable")

    val initialUserData1 = TestData.emptyDomainUserData.copy(userId = "first")

    val initialUserData2 = TestData.emptyDomainUserData.copy(
      userId = "second",
      savedSearches = Some(Seq("Seiddit", "Emina")),
      latestEditedArticles = Some(Seq("article:6", "article:9")),
      favoriteSubjects = Some(Seq("methematics", "PEBCAK-studies"))
    )

    val inserted1 = repository.insert(initialUserData1)
    val inserted2 = repository.insert(initialUserData2)

    val updatedUserData1 = inserted1.get.copy(savedSearches = Some(Seq("1", "2")),
                                              latestEditedArticles = Some(Seq("3", "4")),
                                              favoriteSubjects = Some(Seq("5", "6")))

    val updatedUserData2 = inserted2.get.copy(savedSearches = Some(Seq("a", "b")),
                                              latestEditedArticles = None,
                                              favoriteSubjects = Some(Seq.empty))

    val res1 = repository.update(updatedUserData1)
    val res2 = repository.update(updatedUserData2)

    res1.get should be(repository.withUserId("first").get)
    res2.get should be(repository.withUserId("second").get)
  }

  test("that userDataCount returns correct amount of entries") {
    assume(databaseIsAvailable, "Database is unavailable")

    val data1 = TestData.emptyDomainUserData.copy(userId = "user1")
    val data2 = TestData.emptyDomainUserData.copy(userId = "user2")
    val data3 = TestData.emptyDomainUserData.copy(userId = "user2")

    repository.userDataCount should be(0)
    repository.insert(data1)
    repository.userDataCount should be(1)
    repository.insert(data2)
    repository.userDataCount should be(2)
    try repository.insert(data3) match {
      case Success(_)                => fail()
      case Failure(_: PSQLException) => // ignore results
    }
    repository.userDataCount should be(2)
  }

}
