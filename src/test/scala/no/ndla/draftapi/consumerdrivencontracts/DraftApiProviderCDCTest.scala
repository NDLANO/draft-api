/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.consumerdrivencontracts

import java.io.IOException
import java.net.ServerSocket

import com.itv.scalapact.ScalaPactVerify._
import com.itv.scalapact.shared.{BasicAuthenticationCredentials, BrokerPublishData, ProviderStateResult, TaggedConsumer}
import no.ndla.draftapi._
import org.eclipse.jetty.server.Server
import org.scalatest.Tag
import scalikejdbc._

import scala.sys.process._
import scala.util.Properties.{envOrElse, envOrNone}
import scala.util.Try

object PactProviderTest extends Tag("PactProviderTest")

class DraftApiProviderCDCTest extends IntegrationSuite with TestEnvironment {

  import com.itv.scalapact.argonaut62._
  import com.itv.scalapact.http4s16a._

  def findFreePort: Int = {
    def closeQuietly(socket: ServerSocket): Unit = {
      try {
        socket.close()
      } catch { case _: Throwable => }
    }
    var socket: ServerSocket = null
    try {
      socket = new ServerSocket(0)
      socket.setReuseAddress(true)
      val port = socket.getLocalPort
      closeQuietly(socket)
      return port;
    } catch {
      case e: IOException =>
        logger.trace("Failed to open socket", e);
    } finally {
      if (socket != null) {
        closeQuietly(socket)
      }
    }
    throw new IllegalStateException("Could not find a free TCP/IP port to start embedded Jetty HTTP Server on");
  }

  var server: Option[Server] = None
  val serverPort: Int = findFreePort

  def deleteSchema(): Unit = {
    println("Deleting test schema to prepare for CDC testing...")
    val datasource = testDataSource
    DBMigrator.migrate(datasource)
    ConnectionPool.singleton(new DataSourceConnectionPool(datasource))
    DB autoCommit (implicit session => {
      sql"drop schema if exists articleapitest cascade;"
        .execute()
        .apply()
    })
    DBMigrator.migrate(datasource)
    ConnectionPool.singleton(new DataSourceConnectionPool(datasource))
  }

  override def beforeAll(): Unit = {
    println(s"Running CDC tests with component on localhost:$serverPort")
    server = Some(JettyLauncher.startServer(serverPort))
  }

  private def setupArticles() =
    (1 to 10)
      .map(id => {
        ComponentRegistry.draftRepository.insert(TestData.sampleDomainArticle.copy(id = Some(id)))
      })

  private def setupConcepts() =
    (1 to 10)
      .map(id => {
        ComponentRegistry.conceptRepository.insertWithExternalId(TestData.sampleConcept.copy(id = Some(id)), s"4444$id")
      })

  private def setupAgreements() =
    (1 to 10)
      .map(id => {
        ComponentRegistry.agreementRepository.insert(TestData.sampleBySaDomainAgreement.copy(id = Some(id)))
      })

  override def afterAll(): Unit = server.foreach(_.stop())

  private def getGitVersion =
    for {
      shortCommit <- Try("git rev-parse --short HEAD".!!.trim)
      dirtyness <- Try("git status --porcelain".!!.trim != "").map {
        case true  => "-dirty"
        case false => ""
      }
    } yield s"$shortCommit$dirtyness"

  test("That pacts from broker are working.", PactProviderTest) {
    val isTravis = envOrElse("TRAVIS", "false").toBoolean
    val isPullRequest = envOrElse("TRAVIS_PULL_REQUEST", "false").toBoolean
    val publishResults = if (isTravis && !isPullRequest) {
      getGitVersion.map(version => BrokerPublishData(version, None)).toOption
    } else { None }

    val consumersToVerify = List(
      TaggedConsumer("article-api", List("master"))
    )

    val broker = for {
      url <- envOrNone("PACT_BROKER_URL")
      username <- envOrNone("PACT_BROKER_USERNAME")
      password <- envOrNone("PACT_BROKER_PASSWORD")
      broker <- pactBrokerWithTags(url,
                                   "draft-api",
                                   publishResults,
                                   consumersToVerify,
                                   Some(BasicAuthenticationCredentials(username, password)))
    } yield broker

    broker match {
      case Some(b) =>
        verifyPact
          .withPactSource(b)
          .setupProviderState("given") {
            case "articles"   => deleteSchema(); ProviderStateResult(setupArticles().nonEmpty)
            case "concepts"   => deleteSchema(); ProviderStateResult(setupConcepts().nonEmpty)
            case "agreements" => deleteSchema(); ProviderStateResult(setupAgreements().nonEmpty)
            case "empty"      => deleteSchema(); ProviderStateResult(true)
          }
          .runStrictVerificationAgainst("localhost", serverPort)
      case None => throw new RuntimeException("Could not get broker settings...")
    }
  }
}
