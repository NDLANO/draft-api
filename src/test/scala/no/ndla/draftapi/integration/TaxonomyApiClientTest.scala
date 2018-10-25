/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.integration

import no.ndla.draftapi.model.domain.ArticleTitle
import no.ndla.draftapi.{TestData, TestEnvironment, UnitSuite}
import org.json4s.Formats
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock

import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.{Failure, Success}

class TaxonomyApiClientTest extends UnitSuite with TestEnvironment {

  override val taxonomyApiClient: TaxonomyApiClient = spy(new TaxonomyApiClient)

  override protected def beforeEach(): Unit = {
    // Since we use spy, we reset the mock for each test allowing verify to be accurate
    reset(taxonomyApiClient)
  }

  test("That updating one resources translations works as expected") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val resource = Resource("urn:resource:1:12312", "Outdated name", Some(s"urn:article:$id"))

    // format: off
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Resource](1)))).when(taxonomyApiClient).putRaw(any[String], any[Resource], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    doReturn(Success(List(resource)), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Success(List.empty), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    taxonomyApiClient.updateTaxonomyIfExists(id, article) should be(Success(id))

    verify(taxonomyApiClient, times(1)).updateResource(eqTo(resource.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(2)).updateResourceTranslation(anyString, anyString, anyString)(any[ExecutionContext])

    verify(taxonomyApiClient, times(0)).updateTopic(any[Topic])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateTopicTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    // format: on
  }

  test("That updating one topics translations works as expected") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val topic = Topic("urn:topic:1:12312", "Outdated name", Some(s"urn:article:$id"))

    // format: off
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Topic](1)))) .when(taxonomyApiClient) .putRaw(any[String], any[Topic], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))) .when(taxonomyApiClient) .putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    doReturn(Success(List(topic)), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)
    doReturn(Success(List.empty), Success(List.empty)).when(taxonomyApiClient).queryResource(id)

    taxonomyApiClient.updateTaxonomyIfExists(id, article) should be(Success(id))

    verify(taxonomyApiClient, times(1)).updateTopic(eqTo(topic.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(2)).updateTopicTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateResource(any[Resource])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateResourceTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    // format: on
  }

  test("That updating multiple resources translations works as expected") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val resource1 = Resource("urn:resource:1:12312", "Outdated name", Some(s"urn:article:$id"))
    val resource2 = Resource("urn:resource:1:99551", "Outdated other name", Some(s"urn:article:$id"))

    // format: off
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Resource](1)))).when(taxonomyApiClient).putRaw(any[String], any[Resource], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    doReturn(Success(List(resource1, resource2)), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Success(List.empty), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    taxonomyApiClient.updateTaxonomyIfExists(id, article) should be(Success(id))

    verify(taxonomyApiClient, times(1)).updateResource(eqTo(resource1.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResource(eqTo(resource2.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(2)).updateResource(any[Resource])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource1.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource1.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource2.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource2.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(4)).updateResourceTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateTopic(any[Topic])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateTopicTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    // format: on
  }
  test("That updating multiple topics translations works as expected") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val topic1 = Topic("urn:topic:1:12312", "Outdated name", Some(s"urn:article:$id"))
    val topic2 = Topic("urn:topic:1:99551", "Outdated other name", Some(s"urn:article:$id"))

    // format: off
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Topic](1)))).when(taxonomyApiClient).putRaw(any[String], any[Topic], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    doReturn(Success(List.empty), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Success(List(topic1, topic2)), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    taxonomyApiClient.updateTaxonomyIfExists(id, article) should be(Success(id))

    verify(taxonomyApiClient, times(1)).updateTopic(eqTo(topic1.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopic(eqTo(topic2.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(2)).updateTopic(any[Topic])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic1.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic1.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic2.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic2.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(4)).updateTopicTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateResource(any[Resource])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateResourceTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    // format: on
  }

  test("That both resources and topics for single article is updated") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val resource1 = Resource("urn:resource:1:12035", "Outdated res name", Some(s"urn:article:$id"))
    val resource2 = Resource("urn:resource:1:d8a19b97-10ee-481a-b44c-dd54cffbddda",
                             "Outdated other res name",
                             Some(s"urn:article:$id"))
    val topic1 = Topic("urn:topic:1:12312", "Outdated top name", Some(s"urn:article:$id"))
    val topic2 = Topic("urn:topic:1:99551", "Outdated other top name", Some(s"urn:article:$id"))

    // format: off
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Topic](1)))).when(taxonomyApiClient).putRaw(any[String], any[Topic], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Resource](1)))).when(taxonomyApiClient).putRaw(any[String], any[Resource], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    doReturn(Success(List(resource1, resource2)), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Success(List(topic1, topic2)), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    taxonomyApiClient.updateTaxonomyIfExists(id, article) should be(Success(id))

    verify(taxonomyApiClient, times(1)).updateTopic(eqTo(topic1.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopic(eqTo(topic2.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(2)).updateTopic(any[Topic])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic1.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic1.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic2.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateTopicTranslation(eqTo(topic2.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(4)).updateTopicTranslation(anyString, anyString, anyString)(any[ExecutionContext])

    verify(taxonomyApiClient, times(1)).updateResource(eqTo(resource1.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResource(eqTo(resource2.copy(name = "Norsk")))(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(2)).updateResource(any[Resource])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource1.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource1.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource2.id), eqTo("nb"), eqTo("Norsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(1)).updateResourceTranslation(eqTo(resource2.id), eqTo("en"), eqTo("Engelsk"))(any[ExecutionContext])
    verify(taxonomyApiClient, times(4)).updateResourceTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    // format: on
  }

  test("That updateTaxonomyIfExists fails if updating translation fails") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val resource1 = Resource("urn:resource:1:12035", "Outdated res name", Some(s"urn:article:$id"))
    val resource2 = Resource("urn:resource:1:d8a19b97-10ee-481a-b44c-dd54cffbddda",
                             "Outdated other res name",
                             Some(s"urn:article:$id"))
    val topic1 = Topic("urn:topic:1:12312", "Outdated top name", Some(s"urn:article:$id"))
    val topic2 = Topic("urn:topic:1:99551", "Outdated other top name", Some(s"urn:article:$id"))

    // format: off
    doReturn(Success(List(resource1, resource2)), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Success(List(topic1, topic2)), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Topic](1)))).when(taxonomyApiClient).putRaw(any[String], any[Topic], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Resource](1)))).when(taxonomyApiClient).putRaw(any[String], any[Resource], any[(String, String)])(any[Formats], any[ExecutionContext])
    doReturn(Future.failed(new TimeoutException), Future.failed(new TimeoutException)).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    // format: on

    taxonomyApiClient.updateTaxonomyIfExists(id, article).isFailure should be(true)
  }

  test("That updateTaxonomyIfExists fails if updating fetching topics fails") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val resource1 = Resource("urn:resource:1:12035", "Outdated res name", Some(s"urn:article:$id"))
    val resource2 = Resource("urn:resource:1:d8a19b97-10ee-481a-b44c-dd54cffbddda",
                             "Outdated other res name",
                             Some(s"urn:article:$id"))

    // format: off
    doReturn(Success(List(resource1, resource2)), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Failure(new RuntimeException("woawiwa")), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Topic](1)))).when(taxonomyApiClient).putRaw(any[String], any[Topic], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Resource](1)))).when(taxonomyApiClient).putRaw(any[String], any[Resource], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    // format: on

    taxonomyApiClient.updateTaxonomyIfExists(id, article).isFailure should be(true)
  }

  test("That updateTaxonomyIfExists fails if updating fetching resources fails") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get
    val topic1 = Topic("urn:topic:1:12312", "Outdated top name", Some(s"urn:article:$id"))
    val topic2 = Topic("urn:topic:1:99551", "Outdated other top name", Some(s"urn:article:$id"))

    // format: off
    doReturn(Failure(new RuntimeException("woawiwa")), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Success(List(topic1, topic2)), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Topic](1)))).when(taxonomyApiClient).putRaw(any[String], any[Topic], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Resource](1)))).when(taxonomyApiClient).putRaw(any[String], any[Resource], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])
    // format: on

    taxonomyApiClient.updateTaxonomyIfExists(id, article).isFailure should be(true)
  }
  test("That nothing happens (successfully) if no taxonomy exists") {
    val article = TestData.sampleDomainArticle.copy(
      title = Seq(
        ArticleTitle("Norsk", "nb"),
        ArticleTitle("Engelsk", "en")
      ))
    val id = article.id.get

    // format: off
    doReturn(Success(List.empty), Success(List.empty)).when(taxonomyApiClient).queryResource(id)
    doReturn(Success(List.empty), Success(List.empty)).when(taxonomyApiClient).queryTopic(id)

    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Topic](1)))).when(taxonomyApiClient).putRaw(any[String], any[Topic], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Resource](1)))).when(taxonomyApiClient).putRaw(any[String], any[Resource], any[(String, String)])(any[Formats], any[ExecutionContext])
    doAnswer((i: InvocationOnMock) => Future.successful(Success(i.getArgument[Translation](1)))).when(taxonomyApiClient).putRaw(any[String], any[Translation], any[(String, String)])(any[Formats], any[ExecutionContext])

    taxonomyApiClient.updateTaxonomyIfExists(id, article).isSuccess should be(true)

    verify(taxonomyApiClient, times(0)).updateResource(any[Resource])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateTopic(any[Topic])(any[Formats], any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateResourceTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).updateTopicTranslation(anyString, anyString, anyString)(any[ExecutionContext])
    verify(taxonomyApiClient, times(0)).putRaw(anyString, any, any[(String, String)])(any[Formats], any[ExecutionContext])
    // format: on
  }
}
