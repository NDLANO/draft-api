
/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.draftapi

import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.model.domain.{Agreement, Author, Copyright}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import org.joda.time.DateTime
import org.scalatra.servlet.ScalatraListener

import scala.io.Source


object JettyLauncher extends LazyLogging {
  def buildMostUsedTagsCache = {
    ComponentRegistry.readService.getTagUsageMap()
  }

  def main(args: Array[String]) {
    logger.info(Source.fromInputStream(getClass.getResourceAsStream("/log-license.txt")).mkString)
    logger.info("Starting the db migration...")
    val startDBMillis = System.currentTimeMillis()
    DBMigrator.migrate(ComponentRegistry.dataSource)
    logger.info(s"Done db migration, tok ${System.currentTimeMillis() - startDBMillis}ms")

    val startMillis = System.currentTimeMillis()

    buildMostUsedTagsCache
    logger.info(s"Built tags cache in ${System.currentTimeMillis() - startMillis} ms.")

    val context = new ServletContextHandler()
    context setContextPath "/"
    context.addEventListener(new ScalatraListener)
    context.addServlet(classOf[DefaultServlet], "/")
    context.setInitParameter("org.eclipse.jetty.servlet.Default.dirAllowed", "false")

    val server = new Server(DraftApiProperties.ApplicationPort)
    server.setHandler(context)
    server.start

    val startTime = System.currentTimeMillis() - startMillis
    logger.info(s"Started at port ${DraftApiProperties.ApplicationPort} in $startTime ms.")

    val byNcSa = Copyright("by-nc-sa", "Gotham City", List(Author("Forfatter", "DC Comics")), List(), List(), None, None, None)
    val sampleAgreement = Agreement(
      Some(1),
      "title",
      "content",
      byNcSa,
      DateTime.now().minusDays(2).toDate,
      DateTime.now().minusDays(4).toDate,
      "ndla1234"
    )
    val agreement1 = sampleAgreement.copy(id=Some(2), title="Aper får lov", content = "Aper får kjempe seg fremover")
    val agreement2 = sampleAgreement.copy(id=Some(3), title="Ugler er slemme", content = "Ugler er de slemmeste dyrene")
    val agreement3 = sampleAgreement.copy(id=Some(4), title="Tyven stjeler penger", content = "Det er ikke hemmelig at tyven er den som stjeler penger")
    val agreement4 = sampleAgreement.copy(id=Some(5), title="Vi får låne bildene", content = "Vi får låne bildene av kjeltringene")
    val agreement5 = sampleAgreement.copy(id=Some(6), title="Kjeltringer er ikke velkomne", content = "De er slemmere enn kjeft")
    val agreement6 = sampleAgreement.copy(id=Some(7), title="Du er en tyv", content = "Det er du som er tyven")
    val agreement7 = sampleAgreement.copy(id=Some(8), title="Lurerier er ikke lov", content = "Lurerier er bare lov dersom du er en tyv")
    val agreement8 = sampleAgreement.copy(id=Some(9), title="Hvorfor er aper så slemme", content = "Har du blitt helt ape")
    val agreement9 = sampleAgreement.copy(id=Some(10), title="Du er en av dem du", content = "Det er ikke snilt å være en av dem")
    ComponentRegistry.agreementIndexService.indexDocument(agreement1)
    ComponentRegistry.agreementIndexService.indexDocument(agreement2)
    ComponentRegistry.agreementIndexService.indexDocument(agreement3)
    ComponentRegistry.agreementIndexService.indexDocument(agreement4)
    ComponentRegistry.agreementIndexService.indexDocument(agreement5)
    ComponentRegistry.agreementIndexService.indexDocument(agreement6)
    ComponentRegistry.agreementIndexService.indexDocument(agreement7)
    ComponentRegistry.agreementIndexService.indexDocument(agreement8)
    ComponentRegistry.agreementIndexService.indexDocument(agreement9)

    server.join
  }
}
