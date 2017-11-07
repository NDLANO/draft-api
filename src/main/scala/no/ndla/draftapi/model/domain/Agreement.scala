package no.ndla.draftapi.model.domain

import java.util.Date

case class Agreement(
                    content: String,
                    creators: Seq[Author],
                    processors: Seq[Author],
                    rightsholders: Seq[Author],
                    validFrom: Date,
                    validTo: Date
                    ) {

}
