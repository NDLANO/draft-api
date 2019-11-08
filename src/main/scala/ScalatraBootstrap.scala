/*
 * Part of NDLA draft_api.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

import javax.servlet.ServletContext
import no.ndla.draftapi.ComponentRegistry.{
  agreementController,
  draftController,
  fileController,
  healthController,
  internController,
  ruleController,
  resourcesApp
}
import no.ndla.draftapi.DraftSwagger
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {

  implicit val swagger = new DraftSwagger

  override def init(context: ServletContext) {
    context.mount(draftController, "/draft-api/v1/drafts", "drafts")
    context.mount(fileController, "/draft-api/v1/files", "files")
    context.mount(agreementController, "/draft-api/v1/agreements/", "agreements")
    context.mount(ruleController, "/draft-api/v1/rules", "rules")
    context.mount(resourcesApp, "/draft-api/api-docs")
    context.mount(internController, "/intern")
    context.mount(healthController, "/health")
  }

}
