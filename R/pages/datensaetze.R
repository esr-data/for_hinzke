#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction],
  shiny[
    NS, moduleServer,
    fluidPage, h2,
    icon, div, p
  ]
)

#' Missing description
#' @export

module_datensaetze_ui <- function(id = "datensaetze", label = "m_datensaetze") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Datensätze"),
      draw_under_construction()
    )
  )
}
