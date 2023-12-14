#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer,
    fluidPage, tagList, h2
  ]
)

#' Missing description
#' @export

module_datensaetze_ui <- function(id = "datensaetze", label = "m_datensaetze") {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("datensaetze")

    )
  )
}
