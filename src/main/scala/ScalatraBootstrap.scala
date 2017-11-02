/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


import javax.servlet.ServletContext

import no.ndla.draftapi.ComponentRegistry.{internController, draftController, resourcesApp, healthController, conceptController}
import no.ndla.draftapi.DraftSwagger
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {

  implicit val swagger = new DraftSwagger

  override def init(context: ServletContext) {
    context.mount(draftController, "/draft-api/v1/drafts", "drafts")
    context.mount(conceptController, "/draft-api/v1/concepts", "concepts")
    context.mount(resourcesApp, "/draft-api/api-docs")
    context.mount(internController, "/intern")
    context.mount(healthController, "/health")
  }

}
