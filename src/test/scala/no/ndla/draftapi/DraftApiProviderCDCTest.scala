package no.ndla.draftapi

import java.io.IOException
import java.net.ServerSocket

import com.itv.scalapact.ScalaPactVerify._
import com.itv.scalapact.shared.ProviderStateResult
import com.zaxxer.hikari.HikariDataSource
import org.eclipse.jetty.server.Server
import org.mockito.Mockito._
import scalikejdbc._
import no.ndla.draftapi.TestEnvironment

class DraftApiProviderCDCTest extends IntegrationSuite with TestEnvironment {

  import com.itv.scalapact.circe09._
  import com.itv.scalapact.http4s18._

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

    def deleteSchema()(implicit session: DBSession = AutoSession): Unit = {
      println("Deleting test schema to prepare for CDC testing...")
      sql"drop schema if exists draftapitest cascade;"
        .execute()
        .apply()
    }
    deleteSchema()

    println(s"Running CDC tests with component on localhost:$serverPort")
    server = Some(JettyLauncher.startServer(serverPort))

    // Setting up some state for the tests to use
    agreementRepository.insert(TestData.sampleDomainAgreement)
    draftRepository.insert(TestData.sampleDomainArticle)
    conceptRepository.insertWithExternalId(TestData.sampleConcept, "4444")
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
