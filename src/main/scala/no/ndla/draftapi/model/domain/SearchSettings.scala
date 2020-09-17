package no.ndla.draftapi.model.domain

case class SearchSettings(
    query: Option[String],
    withIdIn: List[Long],
    searchLanguage: String,
    license: Option[String],
    page: Int,
    pageSize: Int,
    sort: Sort.Value,
    articleTypes: Seq[String],
    fallback: Boolean,
    grepCodes: Seq[String],
    shouldScroll: Boolean
)
