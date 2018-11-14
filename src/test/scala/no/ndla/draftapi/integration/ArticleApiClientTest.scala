/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import no.ndla.draftapi.model.api.ContentId
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import no.ndla.network.AuthUser
import org.json4s.DefaultFormats
import org.json4s.native.Serialization.write

import scala.util.Success

class ArticleApiClientTest extends UnitSuite with TestEnvironment {
  implicit val formats: DefaultFormats = DefaultFormats
  override val ndlaClient = new NdlaClient

  // Pact CDC imports
  import com.itv.scalapact.ScalaPactForger._
  import com.itv.scalapact.circe09._
  import com.itv.scalapact.http4s18._

  val idResponse = ContentId(1)

  test("allocating an article id should return a long") {
    forgePact
      .between("draft-api")
      .and("article-api")
      .addInteraction(
        interaction
          .description("Allocating an id should return a long")
          .uponReceiving(method = POST,
                         path = "/intern/id/article/allocate",
                         query = None,
                         headers = Map("Authorization" -> TestData.authHeaderWithWriteRole),
                         body = None,
                         matchingRules = None)
          .willRespondWith(
            status = 200,
            headers = Map.empty,
            body = write(idResponse),
            matchingRules = bodyRegexRule("id", "^[0-9]+$") // The id is hard to predict. Just make sure its a number
          )
      )
      .runConsumerTest { mockConfig =>
        AuthUser.setHeader(TestData.authHeaderWithWriteRole)
        val articleApiClient = new ArticleApiClient(mockConfig.baseUrl)
        val Success(id: Long) = articleApiClient.allocateArticleId(List("1234", "4567"), List("1", "2"))
      }
  }
}
