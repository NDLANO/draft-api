/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.CDC

import java.io.IOException
import java.net.ServerSocket

import com.itv.scalapact.ScalaPactVerify._
import com.itv.scalapact.shared.ProviderStateResult
import no.ndla.draftapi._
import org.eclipse.jetty.server.Server
import scalikejdbc._
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._

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

  override def beforeAll(): Unit = {

    def deleteSchema(): Unit = {
      println("Deleting test schema to prepare for CDC testing...")
      val datasource = testDataSource
      DBMigrator.migrate(datasource)
      ConnectionPool.singleton(new DataSourceConnectionPool(datasource))
      DB autoCommit (implicit session => {
        sql"drop schema if exists draftapitest cascade;"
          .execute()
          .apply()
      })
    }
    deleteSchema()

    println(s"Running CDC tests with component on localhost:$serverPort")
    server = Some(JettyLauncher.startServer(serverPort))

    // Setting up some state for the tests to use
    ComponentRegistry.agreementRepository.insert(TestData.sampleBySaDomainAgreement)
    ComponentRegistry.draftRepository.insert(TestData.sampleDomainArticle)
    ComponentRegistry.conceptRepository.insertWithExternalId(TestData.sampleConcept, "4444")
  }

  override def afterAll(): Unit = server.foreach(_.stop())

  test("That pacts from broker are working.") {
    verifyPact
      .withPactSource(pactBroker("http://pact-broker.ndla-local", "draft-api", List("article-api"), None))
      .setupProviderState("given") { _ =>
        ProviderStateResult(true)
      }
      .runStrictVerificationAgainst("localhost", serverPort)
  }

}
