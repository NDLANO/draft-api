package no.ndla.draftapi

import com.itv.scalapact.ScalaPactVerify._
import com.itv.scalapact.shared.ProviderStateResult
import org.eclipse.jetty.server.Server
import org.mockito.Mockito._
import ru.yandex.qatools.embed.postgresql.util.SocketUtil.findFreePort

class VerifyConsumerContractsTest extends IntegrationSuite with TestEnvironment {

  import com.itv.scalapact.circe09._
  import com.itv.scalapact.http4s18._

  var server: Option[Server] = None
  val serverPort: Int = findFreePort()

  override def beforeAll(): Unit = {
    println(s"Running CDC tests with component on localhost:$serverPort")
    server = Some(JettyLauncher.startServer(serverPort))

    // Mocking some state for the tests to use
    when(agreementRepository.withId(1)).thenReturn(TestData.sampleDomainAgreement)
    when(draftRepository.withId(1)).thenReturn(TestData.sampleDomainArticle)
    when(conceptRepository.withId(1)).thenReturn(TestData.sampleConcept)
  }

  override def afterAll(): Unit = server.foreach(_.stop())

  test("That pacts from broker are working.") {
    verifyPact
      .withPactSource(pactBroker("http://pact-broker.ndla-local", "draft-api", List("article-api"), None))
      .setupProviderState("given") { _ =>
        ProviderStateResult(true)
      }
      .runVerificationAgainst("localhost", serverPort)
  }

}
